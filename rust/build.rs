use std::process::Command;

fn main() {
    // Only add rpath for prolog feature
    #[cfg(feature = "prolog")]
    {
        // Get SWI-Prolog library directory dynamically
        if let Ok(output) = Command::new("swipl")
            .arg("--dump-runtime-variables")
            .output()
        {
            let output_str = String::from_utf8_lossy(&output.stdout);

            // Extract PLLIBDIR from output
            // Format: PLLIBDIR="/usr/lib/swi-prolog/lib/x86_64-linux";
            if let Some(line) = output_str.lines().find(|l| l.starts_with("PLLIBDIR=")) {
                let lib_dir = line
                    .trim_start_matches("PLLIBDIR=\"")
                    .trim_end_matches("\";")
                    .trim_end_matches('"');

                // Add rpath so the runtime linker can find libswipl.so
                println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_dir);
            }
        }
    }
}
