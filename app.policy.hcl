namespace "default" {
  policy = "deny"

  variables {
    path "nomad/jobs/user-count" {
      capabilities = ["write"]
    }
  }
}
