import json
from datetime import datetime
import sys

def calculate_age(dob_str):
    """Simple age calculation if you ever get DOB. For now we use the 'age' field directly."""
    return None  # We'll use the provided 'age' field

def convert_run_signup_to_race_format(input_path, output_path):
    """
    Converts RunSignup results JSON → header + athletes array (human-readable)
    """
    with open(input_path, 'r', encoding='utf-8') as f:
        data = json.load(f)

    # Navigate to the first (and usually only) result set
    result_sets = data.get("individual_results_sets", [])
    if not result_sets:
        raise ValueError("No individual_results_sets found in the file")

    result_set = result_sets[0]  # "All Results" usually

    # Build the header (you can add/remove fields as needed)
    header = {
        "date": result_set.get("event_date") or "unknown-date",   # adjust if date is elsewhere
        "race-name": result_set.get("individual_result_set_name", "Race Results"),
        "source": result_set.get("results_source_name"),
        "race-points": 50
        # Add more metadata here if you want (distance, location, etc.)
    }

    # Extract and clean the athletes/results
    results = result_set.get("results", [])

    athletes = []
    for r in results:
        athlete = {
            "place": r.get("place"),
            "bib": r.get("bib"),
            "name": f"{r.get('first_name', '')} {r.get('last_name', '')}".strip(),
            "sex": r.get("gender"),
            "age": r.get("age"),                    # direct from the data
            "chip_time": r.get("chip_time"),
        }
        athletes.append(athlete)

    # Write the output file in the desired two-part format
    with open(output_path, 'w', encoding='utf-8') as f:
        # 1. Pretty-printed header (first JSON object)
        json.dump(header, f)
        f.write("\n\n")                     # nice visual separation for humans

        # 2. Pretty-printed athletes array (second JSON value)
        json.dump(athletes, f, indent=2)

    print(f"Converted {len(athletes)} athletes.")
    print(f"Header written + athletes array.")
    print(f"Output saved to: {output_path}")


# ============== Usage ==============
if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python convert_race.py <input_signup.json> <output_race.json>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    convert_run_signup_to_race_format(input_file, output_file)

