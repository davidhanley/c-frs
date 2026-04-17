import urllib.request
import urllib.error
import json
from datetime import datetime

RACE_ID = "56563"

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

def is_recent_event(event):
    """Return True if the event is from 2025 or later (recent ones)"""
    event_date = event.get("event_date") or event.get("start_date_time", "")
    if not event_date:
        return True  # Keep if no date (safer)
    
    event_date_str = str(event_date)
    try:
        # Extract year from date string (e.g. "2026-03-08" or "March 8, 2026")
        year = int(event_date_str[:4])
        return year >= 2025
    except:
        return True  # Keep if we can't parse the year

def get_event_results(event):
    """Fetch results for one event and save to JSON"""
    event_id = event.get("event_id")
    event_name = event.get("name", "Unknown_Event").strip()
    event_date = event.get("event_date") or event.get("start_date_time", "")
    
    if not event_id:
        print(f"Skipping event with no ID: {event_name}")
        return
    
    # Build results URL
    url = (f"https://api.runsignup.com/rest/race/{RACE_ID}/results/get-results"
           f"?event_id={event_id}&format=json&results_per_page=2500")
    
    print(f"Fetching results for: {event_name} ({event_date})")
    print(f"   Event ID: {event_id}")
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
    
    # Report approximate number of results
    results_count = "unknown"
    if isinstance(data, dict):
        if "results" in data and isinstance(data.get("results"), list):
            results_count = len(data["results"])
        elif "individual_results" in data and isinstance(data.get("individual_results"), list):
            results_count = len(data["individual_results"])
    
    print(f"   ✅ Saved ~{results_count} results to: {filename}\n")

# ====================== MAIN ======================
if __name__ == "__main__":
    print("=== Starting download of results for RECENT events only (2025+) ===\n")
    
    events = get_all_events()
    
    if not events:
        print("No events found. Exiting.")
    else:
        recent_events = [ev for ev in events if is_recent_event(ev)]
        print(f"Found {len(recent_events)} recent event(s) to process.\n")
        
        for event in recent_events:
            get_event_results(event)
    
    print("Done! Only recent event result JSON files have been saved.")
    print("You can delete any files you don't need.")
