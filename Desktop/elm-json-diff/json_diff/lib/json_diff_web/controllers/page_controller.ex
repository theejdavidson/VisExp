defmodule JsonDiffWeb.PageController do
  use JsonDiffWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
