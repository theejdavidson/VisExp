# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :json_diff,
  ecto_repos: [JsonDiff.Repo]

# Configures the endpoint
config :json_diff, JsonDiffWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "keuxqQG31yI7o6+0jBiN6CgyBv5HEBoXvNa8Ke0XxRX6aCZzFoULGaux/e0R9whM",
  render_errors: [view: JsonDiffWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: JsonDiff.PubSub, adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
