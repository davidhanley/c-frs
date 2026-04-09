import urllib.request
import urllib.error
import json
from datetime import datetime

RACE_ID = "178499"

def fetch_json(url):
    """Helper function to fetch JSON with standard library"""
    try:
        with urllib.request.urlopen(url, timeout=30) as resp:
            return json.loads(resp.read().decode('utf-8'))
    except urllib.error.HTTPError as e:
        print(f"HTTP Error {e.code} for {url}: {e.reason}")
        return None
    except Exception as e:
        print(f"Error fetching {url}: {e}")
        return None

def get_all_events():
    """Fetch the list of all events for the race"""
    url = f"https://api.runsignup.com/rest/race/{RACE_ID}?format=json"
    print(f"Fetching list of all events...\nURL: {url}")
    
    data = fetch_json(url)
    if not data:
        print("Failed to get race events.")
        return []
    
    events = data.get("race", {}).get("events", [])
    print(f"Found {len(events)} events total.\n")
    return events

def get_event_results(event):
    """Fetch results for one event and save to JSON"""
    event_id = event.get("event_id")
    event_name = event.get("name", "Unknown_Event").strip()
    
    if not event_id:
        print(f"Skipping event with no ID: {event_name}")
        return
    
    # Build results URL - gets the main/default result set with all athletes
    url = (f"https://api.runsignup.com/rest/race/{RACE_ID}/results/get-results"
           f"?event_id={event_id}&format=json")
    
    print(f"Fetching results for: {event_name} (Event ID: {event_id})")
    print(f"   URL: {url}")
    
    data = fetch_json(url)
    if not data:
        print(f"   Failed to fetch results for {event_name}\n")
        return
    
    # Create a safe filename
    safe_name = "".join(c if c.isalnum() or c in " _-" else "_" for c in event_name)
    safe_name = safe_name.replace(" ", "_").lower()
    filename = f"results_{RACE_ID}_event_{event_id}_{safe_name}.json"
    
    # Save the full JSON response
    with open(filename, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    
    # Try to report how many results we got
    results_count = "unknown"
    if isinstance(data, dict):
        # Common structures: look for results list or similar
        if "results" in data and isinstance(data["results"], list):
            results_count = len(data["results"])
        elif "individual_results" in data and isinstance(data["individual_results"], list):
            results_count = len(data["individual_results"])
    
    print(f"   ✅ Saved ~{results_count} results to: {filename}\n")

# ====================== MAIN ======================
if __name__ == "__main__":
    print("=== Starting download of results for ALL events ===\n")
    
    events = get_all_events()
    
    if not events:
        print("No events found. Exiting.")
    else:
        for event in events:
            get_event_results(event)
    
    print("Done! All event result JSON files are in the current folder.")
    print("You can delete any files you don't need.") 
