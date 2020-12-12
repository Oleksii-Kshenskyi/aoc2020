defmodule Day4 do
  @moduledoc """
  Documentation for `Day4`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Day4.hello()
      :world

  """
  def hello do
    :world
  end

  def file_to_list(filename) do
    File.read(filename)
    |> Tuple.to_list
    |> Enum.at(1)
    |> String.split("\n", trim: true)
  end

  def main([filename]) do
    filename
    |> file_to_list
    |> IO.inspect
  end
end
