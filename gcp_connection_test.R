# GCP Data Pipeline Connection Test
# Load libraries for cloud data access
library(googleCloudStorageR)
library(googleAuthR)

# Set up authentication using service account
Sys.setenv("GCS_AUTH_FILE" = "gcp-credentials.json")
gcs_auth(json_file = "gcp-credentials.json")

# Connect to your data bucket
bucket_name <- "bucket_flight_data"

# Test connection - list all objects in your bucket
print("Connecting to GCP data bucket...")
bucket_contents <- gcs_list_objects(bucket_name)
print(bucket_contents)

# This will show us what datasets are available in your data lake
print(paste("Found", nrow(bucket_contents), "files in your data bucket"))
