pub mod cart;
pub mod cpu;
pub mod gfx;
pub mod input;
pub mod mem;
pub mod timer;

use crate::{
    cart::Cartridge,
    cpu::Cpu,
    input::{Button, Input},
    mem::Mmu,
};

use gfx::Gpu;
use imgui::{Context, TableColumnSetup, TreeNodeFlags, Ui};
use imgui_glow_renderer::{
    glow::{self, HasContext},
    AutoRenderer,
};
use imgui_sdl2_support::SdlPlatform;
use sdl2::{event::Event, keyboard::Keycode, video::Window};
use std::{cell::RefCell, rc::Rc};

/// Contains the memory address the program counter should be set to after the boot screen is
/// displayed in order to start executing the cartridge instructions.
const _START_INSTRUCTION: u16 = 0x0100;

/// Enumerates the different states that the emulator can be in when using the debug functionality.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DebugMode {
    /// Debugging is disabled, game runs as normal.
    Disabled,
    /// Game execution is paused, the cpu does not step forward.
    Pause,
    /// Game execution will will take one step forward and pause afterwards.
    Step,
}

/// The [`Emulator`] struct is the container that is responsible for managing all of the subsystems
/// required to emulate the Game Boy and play a game cartridge.
pub struct Emulator {
    /// [`Cpu`] that is responsible for reading, decoding and executing instructions.
    pub cpu: Cpu,
    /// [`Mmu`] that is used to store data to and load various types of data.
    pub mmu: Mmu,
    /// [`Cartridge`] that is currently loaded into the emulator.
    pub cartridge: Cartridge,
    /// [`Input`] that allows a user to give input to the emulator.
    pub input: Rc<RefCell<Input>>,
    /// [`Gpu`] that draws the game to the screen.
    pub gpu: Rc<RefCell<Gpu>>,
    /// Current debug mode of the emulator.
    pub debug_mode: DebugMode,
}

impl Emulator {
    /// Creates a new [`Emulator`] which is capable of playing the specified [`Cartridge`].
    pub fn new(cartridge: Cartridge, debug_mode: DebugMode) -> Self {
        let cpu = Cpu::new();
        let input = Rc::new(RefCell::new(Input::new()));
        let gpu = Rc::new(RefCell::new(Gpu::new()));

        let mmu = Mmu::new(
            Rc::clone(&cartridge.mbc),
            Rc::clone(&input),
            Rc::clone(&gpu),
        );

        Self {
            cpu,
            mmu,
            cartridge,
            input,
            gpu,
            debug_mode,
        }
    }
    /// Runs the emulator.
    pub fn run(&mut self) -> anyhow::Result<()> {
        let sdl_context = match sdl2::init() {
            Err(msg) => anyhow::bail!(msg),
            Ok(ctx) => ctx,
        };

        let video_subsystem = match sdl_context.video() {
            Err(msg) => anyhow::bail!(msg),
            Ok(video_subsystem) => video_subsystem,
        };

        #[cfg(target_os = "macos")]
        video_subsystem
            .gl_attr()
            .set_context_profile(sdl2::video::GLProfile::Core);

        #[cfg(target_os = "macos")]
        video_subsystem.gl_attr().set_context_version(3, 3);

        let window_title = format!(
            "GameBoy Emulator - {} [{}]",
            self.cartridge.header.title, self.cartridge.name
        );

        let window = match video_subsystem
            .window(&window_title, 1280, 576)
            .allow_highdpi()
            .opengl()
            .position_centered()
            .build()
        {
            Err(msg) => anyhow::bail!(msg),
            Ok(window) => window,
        };

        let gl_context = window.gl_create_context().expect("opengl context created");

        window
            .gl_make_current(&gl_context)
            .expect("opengl context activated");

        window
            .subsystem()
            .gl_set_swap_interval(1)
            .expect("opengl swap interval set");

        let gl = glow_context(&window);

        let mut imgui = Context::create();

        imgui.set_ini_filename(None);
        imgui.set_log_filename(None);

        imgui
            .fonts()
            .add_font(&[imgui::FontSource::DefaultFontData { config: None }]);

        let mut platform = SdlPlatform::new(&mut imgui);
        let mut renderer = AutoRenderer::new(gl, &mut imgui).expect("renderer created");
        let gl = Rc::clone(renderer.gl_context());

        let vertex_array = unsafe {
            gl.create_vertex_array()
                .expect("Cannot create vertex array")
        };

        let program = unsafe { gl.create_program().expect("program created") };

        let shaders = Vec::from([
            (glow::VERTEX_SHADER, VERTEX_SHADER_SOURCE),
            (glow::FRAGMENT_SHADER, FRAGMENT_SHADER_SOURCE),
        ]);

        let mut compiled_shaders = Vec::with_capacity(shaders.len());

        for (shader_type, shader_src) in shaders.iter() {
            let shader = unsafe {
                let shader = gl.create_shader(*shader_type).expect("shader created");

                gl.shader_source(shader, shader_src);
                gl.compile_shader(shader);

                if !gl.get_shader_compile_status(shader) {
                    anyhow::bail!("compile shader: {}", gl.get_shader_info_log(shader));
                }

                gl.attach_shader(program, shader);

                shader
            };

            compiled_shaders.push(shader);
        }

        unsafe {
            gl.link_program(program);
            if !gl.get_program_link_status(program) {
                anyhow::bail!("link program: {}", gl.get_program_info_log(program));
            }

            for shader in compiled_shaders {
                gl.detach_shader(program, shader);
                gl.delete_shader(shader);
            }

            gl.use_program(Some(program));
        }

        let texture = unsafe {
            let texture = gl.create_texture().expect("screen texture created");

            gl.bind_texture(glow::TEXTURE_2D, Some(texture));

            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MIN_FILTER,
                glow::NEAREST as i32,
            );
            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MAG_FILTER,
                glow::LINEAR as i32,
            );

            texture
        };

        let mut event_pump = match sdl_context.event_pump() {
            Err(msg) => anyhow::bail!(msg),
            Ok(event_pump) => event_pump,
        };

        let mut texture_scroller: usize = 0;

        'main: loop {
            let ticks = match self.debug_mode {
                DebugMode::Disabled => self.cpu.step(&mut self.mmu),
                DebugMode::Pause => 0,
                DebugMode::Step => {
                    self.debug_mode = DebugMode::Pause;
                    self.cpu.step(&mut self.mmu)
                }
            };

            if ticks > 0 {
                // TODO: run other systems for ticks number of cycles
            }

            for event in event_pump.poll_iter() {
                platform.handle_event(&mut imgui, &event);

                match event {
                    // quit
                    Event::Quit { .. }
                    | Event::KeyUp {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'main,

                    // d-pad
                    Event::KeyDown {
                        keycode: Some(Keycode::W),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Up),
                    Event::KeyUp {
                        keycode: Some(Keycode::W),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Up),
                    Event::KeyDown {
                        keycode: Some(Keycode::S),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Down),
                    Event::KeyUp {
                        keycode: Some(Keycode::S),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Down),
                    Event::KeyDown {
                        keycode: Some(Keycode::A),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Left),
                    Event::KeyUp {
                        keycode: Some(Keycode::A),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Left),
                    Event::KeyDown {
                        keycode: Some(Keycode::D),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Right),
                    Event::KeyUp {
                        keycode: Some(Keycode::D),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Right),

                    // buttons
                    Event::KeyDown {
                        keycode: Some(Keycode::I),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Start),
                    Event::KeyUp {
                        keycode: Some(Keycode::I),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Start),
                    Event::KeyDown {
                        keycode: Some(Keycode::U),
                        ..
                    } => self.input.borrow_mut().button_down(Button::Select),
                    Event::KeyUp {
                        keycode: Some(Keycode::U),
                        ..
                    } => self.input.borrow_mut().button_up(Button::Select),
                    Event::KeyDown {
                        keycode: Some(Keycode::K),
                        ..
                    } => self.input.borrow_mut().button_down(Button::A),
                    Event::KeyUp {
                        keycode: Some(Keycode::K),
                        ..
                    } => self.input.borrow_mut().button_up(Button::A),
                    Event::KeyDown {
                        keycode: Some(Keycode::J),
                        ..
                    } => self.input.borrow_mut().button_down(Button::B),
                    Event::KeyUp {
                        keycode: Some(Keycode::J),
                        ..
                    } => self.input.borrow_mut().button_up(Button::B),

                    // debug
                    Event::KeyUp {
                        keycode: Some(Keycode::P),
                        ..
                    } => self.debug_mode = DebugMode::Pause,
                    Event::KeyUp {
                        keycode: Some(Keycode::N),
                        ..
                    } => self.debug_mode = DebugMode::Step,
                    Event::KeyUp {
                        keycode: Some(Keycode::C),
                        ..
                    } => self.debug_mode = DebugMode::Disabled,

                    // not mapped
                    _ => {}
                }
            }

            platform.prepare_frame(&mut imgui, &window, &event_pump);

            self.render_imgui(imgui.new_frame());

            let draw_data = imgui.render();

            unsafe { gl.clear(glow::COLOR_BUFFER_BIT) };

            renderer.render(draw_data).expect("imgui ui rendered");

            // for now just write color palette to the screen texture
            let mut pixel_data = vec![u8::MAX; 640 * 576 * 3];
            for row in 0..576 {
                for col in 0..640 {
                    let col_scroller = (col + texture_scroller) % 640;

                    let (r, g, b) = if col_scroller < 160 {
                        (155, 188, 15)
                    } else if col_scroller < 320 {
                        (139, 172, 15)
                    } else if col_scroller < 480 {
                        (48, 98, 48)
                    } else {
                        (15, 56, 15)
                    };

                    let base = (3 * row * 640) + (col * 3);
                    pixel_data[base] = r;
                    pixel_data[base + 1] = g;
                    pixel_data[base + 2] = b;
                }
            }

            unsafe {
                gl.bind_texture(glow::TEXTURE_2D, Some(texture));

                gl.tex_image_2d(
                    glow::TEXTURE_2D,
                    0,
                    glow::RGB8 as i32,
                    640,
                    576,
                    0,
                    glow::RGB,
                    glow::UNSIGNED_BYTE,
                    Some(&pixel_data),
                );

                gl.bind_vertex_array(Some(vertex_array));
                gl.draw_arrays(glow::TRIANGLES, 0, 6)
            }

            window.gl_swap_window();

            texture_scroller = texture_scroller.wrapping_add(1);
        }

        tracing::debug!("destroying opengl resources");

        unsafe {
            renderer.gl_context().delete_texture(texture);
            renderer.gl_context().delete_program(program);
            renderer.gl_context().delete_vertex_array(vertex_array);
        };

        tracing::debug!("exiting emulator");

        Ok(())
    }
    /// Draws the ImGUI UI for the frame based on the state of the emulator.
    fn render_imgui(&mut self, ui: &mut Ui) {
        if let Some(left_panel_win_token) = ui
            .window("Left Panel")
            .no_decoration()
            .size([320.0, 576.0], imgui::Condition::FirstUseEver)
            .position([0.0, 0.0], imgui::Condition::FirstUseEver)
            .begin()
        {
            if ui.collapsing_header("Cartridge", TreeNodeFlags::DEFAULT_OPEN) {
                ui.text(format!("Title: {}", self.cartridge.header.title));
                ui.text(format!(
                    "Licensee Code: {}",
                    self.cartridge.header.licensee_code()
                ));
                ui.text(format!("Version: {}", self.cartridge.header.version));
                ui.text(format!("Type: {}", self.cartridge.header.kind));
                ui.separator();
                if let Some(checksums_table) = ui.begin_table_header(
                    "checksums_table",
                    [
                        TableColumnSetup::new("Checksum"),
                        TableColumnSetup::new("Expected"),
                        TableColumnSetup::new("Calculated"),
                    ],
                ) {
                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text("Header");
                    ui.table_next_column();
                    ui.text(format!(
                        "{}",
                        self.cartridge.header.expected_header_checksum
                    ));
                    ui.table_next_column();
                    ui.text(format!(
                        "{}",
                        self.cartridge.header.computed_header_checksum
                    ));

                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text("Global");
                    ui.table_next_column();
                    ui.text(format!(
                        "{}",
                        self.cartridge.header.expected_global_checksum,
                    ));
                    ui.table_next_column();
                    ui.text(format!(
                        "{}",
                        self.cartridge.header.computed_global_checksum
                    ));

                    checksums_table.end();
                }
            }

            if ui.collapsing_header("CPU", TreeNodeFlags::DEFAULT_OPEN) {
                if let Some(pc_sp_table) = ui.begin_table("pc_sp_table", 2) {
                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("PC: {}", self.cpu.registers.pc));
                    ui.table_next_column();
                    ui.text(format!("SP: {}", self.cpu.registers.sp));

                    pc_sp_table.end();
                }

                ui.separator();

                if let Some(registers_table) = ui.begin_table("registers_table", 4) {
                    ui.table_next_row();

                    ui.table_next_column();
                    ui.text(format!("A: {}", self.cpu.registers.a));
                    ui.table_next_column();
                    ui.text(format!("B: {}", self.cpu.registers.b));
                    ui.table_next_column();
                    ui.text(format!("C: {}", self.cpu.registers.c));
                    ui.table_next_column();
                    ui.text(format!("D: {}", self.cpu.registers.d));

                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("E: {}", self.cpu.registers.e));
                    ui.table_next_column();
                    ui.text(format!("F: {}", self.cpu.registers.f));
                    ui.table_next_column();
                    ui.text(format!("H: {}", self.cpu.registers.h));
                    ui.table_next_column();
                    ui.text(format!("L: {}", self.cpu.registers.l));

                    registers_table.end();
                }

                ui.separator();

                if let Some(flags_table) = ui.begin_table("flags_table", 4) {
                    ui.table_next_row();

                    ui.table_next_column();
                    ui.text(format!("Z: {}", self.cpu.registers.f.z()));
                    ui.table_next_column();
                    ui.text(format!("N: {}", self.cpu.registers.f.n()));
                    ui.table_next_column();
                    ui.text(format!("H: {}", self.cpu.registers.f.h()));
                    ui.table_next_column();
                    ui.text(format!("C: {}", self.cpu.registers.f.c()));

                    flags_table.end();
                }

                ui.separator();

                ui.text(format!("Halted: {}", self.cpu.halted));
                ui.text(format!("Instruction Set: {}", self.cpu.instruction_set));
            }

            if ui.collapsing_header("Interrupt", TreeNodeFlags::DEFAULT_OPEN) {
                ui.text(format!("IME: {}", self.cpu.ime));
                ui.separator();
                ui.text("Enabled");
                if let Some(int_enabled_table) = ui.begin_table("int_enabled_table", 3) {
                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("Joypad: {}", self.mmu.interrupt_enabled.joypad()));
                    ui.table_next_column();
                    ui.text(format!("Serial: {}", self.mmu.interrupt_enabled.serial()));
                    ui.table_next_column();
                    ui.text(format!("Timer: {}", self.mmu.interrupt_enabled.timer()));

                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("LCD: {}", self.mmu.interrupt_enabled.lcd()));
                    ui.table_next_column();
                    ui.text(format!("VBlank: {}", self.mmu.interrupt_enabled.v_blank()));

                    int_enabled_table.end();
                }

                ui.separator();
                ui.text("Requested");

                if let Some(int_requested_table) = ui.begin_table("int_requested_table", 3) {
                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("Joypad: {}", self.mmu.interrupt_flag.joypad()));
                    ui.table_next_column();
                    ui.text(format!("Serial: {}", self.mmu.interrupt_flag.serial()));
                    ui.table_next_column();
                    ui.text(format!("Timer: {}", self.mmu.interrupt_flag.timer()));

                    ui.table_next_row();
                    ui.table_next_column();
                    ui.text(format!("LCD: {}", self.mmu.interrupt_flag.lcd()));
                    ui.table_next_column();
                    ui.text(format!("VBlank: {}", self.mmu.interrupt_flag.v_blank()));

                    int_requested_table.end();
                }
            }

            left_panel_win_token.end();
        }

        if let Some(right_panel_win_token) = ui
            .window("Right Panel")
            .no_decoration()
            .size([320.0, 576.0], imgui::Condition::FirstUseEver)
            .position([960.0, 0.0], imgui::Condition::FirstUseEver)
            .begin()
        {
            if imgui::CollapsingHeader::new("Debug").build(ui) {
                if ui.button("Pause") {
                    self.debug_mode = DebugMode::Pause;
                }
                ui.same_line();
                if ui.button("Step") {
                    self.debug_mode = DebugMode::Step;
                }
                ui.same_line();
                if ui.button("Continue") {
                    self.debug_mode = DebugMode::Disabled;
                }
                ui.same_line();
                if ui.button("Key Bindings") {
                    ui.open_popup("key-bindings");
                }
                if let Some(key_binds_popup) = ui.begin_popup("key-bindings") {
                    ui.text("Input");
                    ui.text("Up: W");
                    ui.text("Down: S");
                    ui.text("Left: A");
                    ui.text("Right: D");
                    ui.text("Select: U");
                    ui.text("Start: I");
                    ui.text("B: J");
                    ui.text("A: K");
                    ui.separator();
                    ui.text("Debug");
                    ui.text("Pause: P");
                    ui.text("Step: N");
                    ui.text("Continue: C");
                    ui.separator();
                    ui.text("Misc");
                    ui.text("Quit: Esc");

                    key_binds_popup.end();
                }
            }

            if ui.collapsing_header("Instructions", TreeNodeFlags::DEFAULT_OPEN) {
                if let Some(last) = self.cpu.history.front() {
                    ui.text(format!("{}", last));
                    ui.separator();
                }

                for inst in self.cpu.history.iter().skip(1) {
                    ui.text(format!("{}", inst));
                }
            }

            right_panel_win_token.end();
        }
    }
}

/// Source code for the OpenGL vertex shader that outputs the screen texture to the window. It
/// hard-codes a static square which the screen texture is mapped onto.
const VERTEX_SHADER_SOURCE: &str = r#"
#version 330 core

const vec2 verts[6] = vec2[6](
    vec2(-0.5f, -1.0f),
    vec2(-0.5f, 1.0f),
    vec2(0.5f, 1.0f),

    vec2(0.5f, 1.0f),
    vec2(0.5f, -1.0f),
    vec2(-0.5f, -1.0f)
);

const vec2 tex_coords[6] = vec2[6](
    vec2(0.0f, 0.0f),
    vec2(0.0f, 1.0f),
    vec2(1.0f, 1.0f),

    vec2(1.0f, 1.0f),
    vec2(1.0f, 0.0f),
    vec2(0.0f, 0.0f)
);

out vec2 tex_coord;

void main() {
    tex_coord = tex_coords[gl_VertexID];
    gl_Position = vec4(verts[gl_VertexID], 0.0, 1.0);
}
"#;

/// Source code for the OpenGL fragment shader that outputs the screen texture to the window. It
/// samples the screen texture that is computed per frame based on the state of the GameBoy.
const FRAGMENT_SHADER_SOURCE: &str = r#"
#version 330 core

uniform sampler2D screen_tex;

in vec2 tex_coord;

out vec4 color;

void main() {
    color = texture(screen_tex, tex_coord);
}
"#;

fn glow_context(window: &Window) -> glow::Context {
    unsafe {
        glow::Context::from_loader_function(|s| window.subsystem().gl_get_proc_address(s) as _)
    }
}
