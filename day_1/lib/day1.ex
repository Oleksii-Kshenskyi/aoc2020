defmodule Day1 do
  @moduledoc """
  Documentation for `Day1`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Day1.hello()
      :world

  """

  def map_of_answers(the_list) do
    the_list
    |> Enum.map(fn first ->
      the_list
      |> Enum.map(fn second ->
        if first != second and first + second == 2020, do: first * second
      end)
    end)
  end

  def which_pairs_product_is_2020(the_list) do
    the_list
    |> map_of_answers
    |> Enum.flat_map(fn elem -> elem end)
    |> Enum.filter(fn elem -> elem != nil end)
    |> List.first
  end


  def main([filename]) do
    File.stream!(filename)
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.to_integer/1)
    |> Enum.to_list
    |> which_pairs_product_is_2020
    |> IO.inspect
  end
end
