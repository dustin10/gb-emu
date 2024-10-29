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
    /// Current debug mode of the emulator.
    pub debug_mode: DebugMode,
}

impl Emulator {
    /// Creates a new [`Emulator`] which is capable of playing the specified [`Cartridge`].
    pub fn new(cartridge: Cartridge, debug_mode: DebugMode) -> Self {
        let cpu = Cpu::new();
        let mmu = Mmu::new(Rc::clone(&cartridge.mbc));

        Self {
            cpu,
            cartridge,
            mmu,
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
                    Event::Quit { .. }
                    | Event::KeyUp {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'main,
                    Event::KeyUp {
                        keycode: Some(Keycode::H),
                        ..
                    } => self.debug_pause(),
                    Event::KeyUp {
                        keycode: Some(Keycode::J),
                        ..
                    } => self.debug_step(),
                    Event::KeyUp {
                        keycode: Some(Keycode::K),
                        ..
                    } => self.debug_continue(),
                    _ => {}
                }
            }

            platform.prepare_frame(&mut imgui, &window, &event_pump);

            let ui = imgui.new_frame();

            self.render_imgui(ui);

            let draw_data = imgui.render();

            unsafe { renderer.gl_context().clear(glow::COLOR_BUFFER_BIT) };
            renderer.render(draw_data).expect("scene can be drawn");

            window.gl_swap_window();
        }

        tracing::debug!("exited main loop");

        Ok(())
    }
    /// Pauses the emulator instruction processing.
    fn debug_pause(&mut self) {
        tracing::debug!("instruction execution paused");
        self.debug_mode = DebugMode::Pause;
    }
    /// Steps the emulator forward one instruction.
    fn debug_step(&mut self) {
        tracing::debug!("stepping forward one instruction");
        self.debug_mode = DebugMode::Step;
    }
    /// Resumes normal emulator execution.
    fn debug_continue(&mut self) {
        tracing::debug!("resuming normal instruction execution");
        self.debug_mode = DebugMode::Disabled;
    }
    /// Draws the ImGUI UI for the frame.
    fn render_imgui(&mut self, ui: &mut Ui) {
        if let Some(left_panel_win_token) = ui
            .window("Left Panel")
            .no_decoration()
            .size([320.0, 720.0], imgui::Condition::FirstUseEver)
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
            }

            if ui.collapsing_header("CPU", TreeNodeFlags::DEFAULT_OPEN) {
                ui.text(format!("PC: {}", self.cpu.registers.pc));
                ui.text(format!("SP: {}", self.cpu.registers.sp));
                ui.separator();
                ui.text(format!("A: {}", self.cpu.registers.a));
                ui.text(format!("B: {}", self.cpu.registers.b));
                ui.text(format!("C: {}", self.cpu.registers.c));
                ui.text(format!("D: {}", self.cpu.registers.d));
                ui.text(format!("E: {}", self.cpu.registers.e));
                ui.text(format!("F: {}", self.cpu.registers.f));
                ui.text(format!("H: {}", self.cpu.registers.h));
                ui.text(format!("L: {}", self.cpu.registers.l));
                ui.separator();
                ui.text(format!("Z: {}", self.cpu.registers.f.z()));
                ui.text(format!("N: {}", self.cpu.registers.f.n()));
                ui.text(format!("H: {}", self.cpu.registers.f.h()));
                ui.text(format!("C: {}", self.cpu.registers.f.c()));
                ui.separator();
                ui.text(format!("Halted: {}", self.cpu.halted));
                ui.text(format!("Interruptable: {}", self.cpu.interruptable));
                ui.text(format!("Instruction Set: {}", self.cpu.instruction_set));
            }

            left_panel_win_token.end();
        }

        if let Some(right_panel_win_token) = ui
            .window("Right Panel")
            .no_decoration()
            .size([320.0, 720.0], imgui::Condition::FirstUseEver)
            .position([980.0, 0.0], imgui::Condition::FirstUseEver)
            .begin()
        {
            if ui.collapsing_header("Debug", TreeNodeFlags::DEFAULT_OPEN) {
                if ui.button("Pause") {
                    self.debug_pause();
                }
                ui.same_line();
                if ui.button("Step") {
                    self.debug_step();
                }
                ui.same_line();
                if ui.button("Continue") {
                    self.debug_continue();
                }
            }

            if ui.collapsing_header("Instructions", TreeNodeFlags::DEFAULT_OPEN) {
                for inst in self.cpu.history.iter().take(40) {
                    ui.text(format!("{}", inst));
                }
            }

            right_panel_win_token.end();
        }
    }
}

fn glow_context(window: &Window) -> glow::Context {
    unsafe {
        glow::Context::from_loader_function(|s| window.subsystem().gl_get_proc_address(s) as _)
    }
}
