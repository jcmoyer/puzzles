const std = @import("std");
const Step = std.build.Step;

const year = 2022;

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const all_tests = b.step("test-all", "Run tests for all days");

    var day: usize = 1;
    while (day <= 25) : (day += 1) {
        const day_name = b.fmt("day{d:0>2}", .{day});
        const src_name = b.fmt("src/{s}.zig", .{day_name});

        std.fs.cwd().access(src_name, .{}) catch |err| {
            if (err == error.FileNotFound) {
                continue;
            } else {
                std.debug.print("Failed to access src_name: {!}", .{err});
                std.process.exit(1);
            }
        };

        const exe = b.addExecutable(day_name, src_name);
        exe.setTarget(target);
        exe.setBuildMode(mode);
        exe.install();

        const year_str = b.fmt("{d}", .{year});
        const day_str = b.fmt("{d}", .{day});
        const get_input = b.addSystemCommand(&[_][]const u8{
            "python",
            "../scripts/aoctool.py",
            "get-input",
            year_str,
            day_str,
        });
        // TODO: maybe could get this from aoctool script
        const input_name = b.fmt("test/{s}-input.txt", .{day_name});
        const output_name = b.fmt("test/{s}-output.txt", .{day_name});

        const run_cmd = exe.run();
        run_cmd.addArg(input_name);

        const run_step_name = b.fmt("run-{s}", .{day_name});
        const run_step_desc = b.fmt("Fetch input and run {s}", .{day_name});
        const run_step = b.step(run_step_name, run_step_desc);
        run_step.dependOn(&get_input.step);
        run_step.dependOn(&run_cmd.step);

        const tests = b.addTest(src_name);
        tests.setTarget(target);
        tests.setBuildMode(mode);
        const test_step_name = b.fmt("test-{s}", .{day_name});
        const test_step_desc = b.fmt("Run tests for {s}", .{day_name});
        const test_step = b.step(test_step_name, test_step_desc);
        test_step.dependOn(&tests.step);
        all_tests.dependOn(&tests.step);

        std.fs.cwd().access(output_name, .{}) catch |err| {
            if (err == error.FileNotFound) {
                // ok, this puzzle probably wasn't solved yet
                continue;
            } else {
                std.debug.print("Failed to stat output_name: {!}", .{err});
                std.process.exit(1);
            }
        };

        const test_runner = PythonTestRunnerStep.init(b, exe, input_name, output_name);
        test_step.dependOn(&test_runner.step);
        all_tests.dependOn(&test_runner.step);
    }
}

const PythonTestRunnerStep = struct {
    builder: *std.build.Builder,
    step: Step,
    exe_step: *std.build.LibExeObjStep,
    input_filename: []const u8,
    output_filename: []const u8,

    fn init(b: *std.build.Builder, exe: *std.build.LibExeObjStep, input_filename: []const u8, output_filename: []const u8) *PythonTestRunnerStep {
        var self = b.allocator.create(PythonTestRunnerStep) catch unreachable;
        self.* = .{
            .builder = b,
            .step = Step.init(.custom, b.fmt("PythonTestRunnerStep{s}", .{exe.name}), b.allocator, make),
            .exe_step = exe,
            .input_filename = input_filename,
            .output_filename = output_filename,
        };
        self.step.dependOn(&exe.step);
        return self;
    }

    fn make(step: *Step) anyerror!void {
        var self = @fieldParentPtr(PythonTestRunnerStep, "step", step);
        const result = try std.ChildProcess.exec(.{
            .allocator = self.builder.allocator,
            .argv = &[_][]const u8{
                "python",
                "../scripts/test_runner.py",
                self.exe_step.getOutputSource().generated.getPath(),
                self.input_filename,
                self.output_filename,
            },
        });
        if (result.term.Exited != 0) {
            std.debug.print("{s}\n", .{result.stdout});
            std.debug.print("{s}\n", .{result.stderr});
            return error.BadTestExitCode;
        }
    }
};
