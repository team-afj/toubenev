import os.path

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

# If modifying these scopes, delete the file token.json.
SCOPES = ["https://www.googleapis.com/auth/spreadsheets.readonly"]

# The ID and range of a sample spreadsheet.
spreadsheetId = "1CzC46w2jpikSu-8xEWo4lAVDXmDRj8JtbHenTa0C_2s"
SAMPLE_RANGE_NAME = "sbénévoles"


def dict_of_list(l: list):
    """
    Build a list of dicts from a list of list where the first list contains
    the keys and the following lists are the values.
    """
    keys = l.pop(0)
    return list(map(lambda row: dict(zip(keys, row)), l))


def main():
    creds = None
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists("token.json"):
        creds = Credentials.from_authorized_user_file("token.json", SCOPES)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file("credentials.json", SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open("token.json", "w") as token:
            token.write(creds.to_json())

    try:
        service = build("sheets", "v4", credentials=creds)

        # Call the Sheets API
        sheet = service.spreadsheets()
        result = (
            sheet.values()
            .batchGet(
                spreadsheetId=spreadsheetId,
                dateTimeRenderOption="SERIAL_NUMBER",
                valueRenderOption="UNFORMATTED_VALUE",
                ranges=["stypes", "slieux", "sbénévoles", "squêtes"],
            )
            .execute()
        )
        values = result.get("valueRanges", [])

        if not values:
            print("No data found.")
            return None

        stypes = values.pop(0)["values"]
        slieux = values.pop(0)["values"]
        sbénévoles = values.pop(0)["values"]
        squêtes = values.pop(0)["values"]

        result = {
            "types_de_quêtes": dict_of_list(stypes),
            "lieux": dict_of_list(slieux),
            "bénévoles": dict_of_list(sbénévoles),
            "quêtes": dict_of_list(squêtes),
        }
        print(result)
        return result
    except HttpError as err:
        print(err)
        return None


if __name__ == "__main__":
    main()
