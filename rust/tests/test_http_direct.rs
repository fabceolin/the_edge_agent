// test to verify HTTP client works
use reqwest;
use serde_json::json;

#[test]
fn test_http_ollama() {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(60))
        .build()
        .expect("Failed to build client");

    let request = json!({
        "model": "gemma3n:e4b",
        "messages": [{"role": "user", "content": "Say hello"}]
    });

    let result = client
        .post("http://localhost:11434/v1/chat/completions")
        .json(&request)
        .send();

    match result {
        Ok(resp) => {
            println!("Status: {}", resp.status());
            assert!(resp.status().is_success());
        }
        Err(e) => {
            println!("Error: {:?}", e);
            panic!("HTTP request failed");
        }
    }
}
