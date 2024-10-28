pub mod cart;
pub mod cpu;
pub mod mem;

use crate::{cart::Cartridge, cpu::Cpu, mem::Mmu};

use imgui::{Context, TreeNodeFlags, Ui};
use imgui_glow_renderer::{
    glow::{self, HasContext},
    AutoRenderer,
};
use imgui_sdl2_support::SdlPlatform;
use sdl2::{event::Event, keyboard::Keycode, video::Window};
use std::rc::Rc;

/// Contains the memory address the program counter should be set to after the boot screen is
/// displayed in order to start executing the cartridge instructions.
const START_INSTRUCTION: u16 = 0x0100;

/// The [`Emulator`] struct is the container that is responsible for managing all of the subsystems
/// required to emulate the Game Boy and play a game cartridge.
pub struct Emulator {
    /// [`Cpu`] that is responsible for reading, decoding and executing instructions.
    cpu: Cpu,
    /// [`Mmu`] that is used to store data to and load various types of data.
    mmu: Mmu,
    /// [`Cartridge`] that is currently loaded into the emulator.
    cartridge: Cartridge,
}

impl Emulator {
    /// Creates a new [`Emulator`] which is capable of playing the specified [`Cartridge`].
    pub fn load(cartridge: Cartridge) -> Self {
        let cpu = Cpu::new();
        let mmu = Mmu::new(Rc::clone(&cartridge.mbc));

        Self {
            cpu,
            cartridge,
            mmu,
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
            .window(&window_title, 1280, 720)
            .allow_highdpi()
            .opengl()
            .position_centered()
            .build()
        {
            Err(msg) => anyhow::bail!(msg),
            Ok(window) => window,
        };

        let gl_context = window
            .gl_create_context()
            .expect("OpenGL context can be created");

        window
            .gl_make_current(&gl_context)
            .expect("OpenGL context can be activated");

        window
            .subsystem()
            .gl_set_swap_interval(1)
            .expect("OpenGL swap interval can be set");

        let gl = glow_context(&window);

        let mut imgui = Context::create();

        imgui.set_ini_filename(None);
        imgui.set_log_filename(None);

        imgui
            .fonts()
            .add_font(&[imgui::FontSource::DefaultFontData { config: None }]);

        let mut platform = SdlPlatform::new(&mut imgui);
        let mut renderer = AutoRenderer::new(gl, &mut imgui).expect("Renderer can be created");

        let mut event_pump = match sdl_context.event_pump() {
            Err(msg) => anyhow::bail!(msg),
            Ok(event_pump) => event_pump,
        };

        self.cpu.registers.pc = START_INSTRUCTION;

        'main: loop {
            let _ticks = self.cpu.step(&mut self.mmu);

            for event in event_pump.poll_iter() {
                platform.handle_event(&mut imgui, &event);

                match event {
                    Event::Quit { .. }
                    | Event::KeyUp {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'main,
                    _ => {}
                }
            }

            platform.prepare_frame(&mut imgui, &window, &event_pump);

            let ui = imgui.new_frame();

            self.render_imgui(ui);
            //ui.show_demo_window(&mut true);

            let draw_data = imgui.render();

            unsafe { renderer.gl_context().clear(glow::COLOR_BUFFER_BIT) };
            renderer.render(draw_data).expect("scene can be drawn");

            window.gl_swap_window();
        }

        tracing::debug!("exited main loop");

        Ok(())
    }
    fn render_imgui(&self, ui: &mut Ui) {
        if let Some(emu_win_token) = ui
            .window("GameBoy Emulator Window")
            .size([1280.0, 720.0], imgui::Condition::FirstUseEver)
            .position([0.0, 0.0], imgui::Condition::FirstUseEver)
            .no_decoration()
            .begin()
        {
            if let Some(left_panel_win_token) = ui
                .window("Left Panel")
                .no_decoration()
                .size([300.0, 720.0], imgui::Condition::FirstUseEver)
                .position([0.0, 0.0], imgui::Condition::FirstUseEver)
                .begin()
            {
                if let Some(cart_tree_node) = ui.tree_node("Cartridge") {
                    ui.text(format!("Title: {}", self.cartridge.header.title));
                    ui.text(format!("Version: {}", self.cartridge.header.version));
                    ui.text(format!("Type: {}", self.cartridge.header.cartridge_type));
                    ui.text(format!(
                        "Header Checksum: Exp {}/Calc {}",
                        self.cartridge.header.expected_header_checksum,
                        self.cartridge.header.computed_header_checksum
                    ));
                    ui.text(format!(
                        "Global Checksum: Exp {}/Calc {}",
                        self.cartridge.header.expected_global_checksum,
                        self.cartridge.header.computed_global_checksum
                    ));

                    cart_tree_node.end();
                }

                if let Some(cpu_reg_tree_node) = ui.tree_node("Registers") {
                    ui.text(format!("pc: {}", self.cpu.registers.pc));
                    ui.text(format!("sp: {}", self.cpu.registers.sp));
                    ui.separator();
                    ui.text(format!("a: {}", self.cpu.registers.a));
                    ui.text(format!("b: {}", self.cpu.registers.b));
                    ui.text(format!("c: {}", self.cpu.registers.c));
                    ui.text(format!("d: {}", self.cpu.registers.d));
                    ui.text(format!("e: {}", self.cpu.registers.e));
                    ui.text(format!("h: {}", self.cpu.registers.h));
                    ui.text(format!("l: {}", self.cpu.registers.l));

                    cpu_reg_tree_node.end();
                }

                left_panel_win_token.end();
            }

            if let Some(right_panel_win_token) = ui
                .window("Right Panel")
                .no_decoration()
                .size([300.0, 720.0], imgui::Condition::FirstUseEver)
                .position([980.0, 0.0], imgui::Condition::FirstUseEver)
                .begin()
            {
                if let Some(inst_tree_node) = ui.tree_node("Instructions") {
                    for inst in self.cpu.history.iter().take(40) {
                        ui.text(format!("{}", inst));
                    }

                    inst_tree_node.end();
                }

                right_panel_win_token.end();
            }

            emu_win_token.end();
        }
    }
}

fn glow_context(window: &Window) -> glow::Context {
    unsafe {
        glow::Context::from_loader_function(|s| window.subsystem().gl_get_proc_address(s) as _)
    }
}
