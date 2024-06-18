provider "aws" {
  region = "us-west-2"
}

variable "tables" {
  description = "List of DynamoDB tables to create"
  type        = list(string)
  default     = ["rooms", "invitations", "user_rooms", "messages", "private_messages"]
}

resource "aws_dynamodb_table" "table" {
  count = length(var.tables)

  name           = var.tables[count.index]
  billing_mode   = "PROVISIONED"
  read_capacity  = 1
  write_capacity = 1
  hash_key       = "id"

  attribute {
    name = "id"
    type = "S"
  }

  attribute {
    name = "data"
    type = "S"
  }
}