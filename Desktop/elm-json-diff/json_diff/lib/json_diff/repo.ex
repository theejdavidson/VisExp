defmodule JsonDiff.Repo do
  use Ecto.Repo,
    otp_app: :json_diff,
    adapter: Ecto.Adapters.Postgres
end
