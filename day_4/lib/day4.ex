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

  def required_fields do
    [
      "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid",
    ]
  end

  def file_to_list_of_inputs(filename) do
    filename
    |> File.read
    |> Tuple.to_list
    |> Enum.at(1)
    |> String.split("\n\n", trim: true)
  end

  def list_of_inputs_to_list_of_passports(the_list) do
    the_list
    |> Enum.map(fn elem -> elem |> String.split([" ", "\n"]) end)
  end

  def passports_to_maps(the_list) do
    for one_pass <- the_list do
      one_pass
      |> Enum.map(fn single_prop ->
        single_prop
        |> String.split(":")
        |> List.to_tuple()
      end)
      |> Enum.into(%{})
    end
  end

  def is_pass_valid?(the_pass, required_fields) do
    required_fields
    |> Enum.all?(fn req_field -> Map.has_key?(the_pass, req_field) end)
  end

  def count_valid_passes(list_of_passes) do
    list_of_passes
    |> Enum.count(fn one_pass ->
      one_pass
      |> is_pass_valid?(required_fields())
    end)
  end

  def main([filename]) do
    filename
    |> file_to_list_of_inputs
    |> list_of_inputs_to_list_of_passports
    |> passports_to_maps
    |> count_valid_passes
    |> IO.inspect
  end
end
