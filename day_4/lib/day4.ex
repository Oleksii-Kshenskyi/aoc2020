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


  def is_byr_valid?(the_byr) do
    the_byri = String.to_integer(the_byr)
    1920 <= the_byri and the_byri <= 2002
  end
  def is_iyr_valid?(the_iyr) do
    the_iyri = String.to_integer(the_iyr)
    2010 <= the_iyri and the_iyri <= 2020
  end
  def is_eyr_valid?(the_eyr) do
    the_eyri = String.to_integer(the_eyr)
    2020 <= the_eyri and the_eyri <= 2030
  end
  def is_hgt_valid?(the_hgt) do
    cond do
      the_hgt |> String.contains?("in") ->
        strnum = the_hgt |> String.slice(0..1)
        if strnum |> String.match?(~r/[[:digit:]]{2}/) do
          int_hgt = the_hgt |> String.slice(0..1) |> String.to_integer
          59 <= int_hgt and int_hgt <= 76
        else
          false
        end
      the_hgt |> String.contains?("cm") ->
        strnum = the_hgt |> String.slice(0..2)
        if strnum |> String.match?(~r/[[:digit:]]{3}/) do
          int_hgt = strnum |> String.to_integer
          150 <= int_hgt and int_hgt <= 193
        else
          false
        end
      true -> false
    end
  end
  def is_hcl_valid?(the_hcl) do
    the_hcl |> String.match?(~r/^\#([[:xdigit:]]){6}$/)
  end
  def ecl_options do
    [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
  end
  def is_ecl_valid?(the_ecl) do
    ecl_options()
    |> Enum.any?(fn option -> option == the_ecl end)
  end
  def is_pid_valid?(the_pid) do
    the_pid |> String.match?(~r/^[[:digit:]]{9}$/)
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

  def required_checks do
    %{
      "byr" => &is_byr_valid?/1,
      "iyr" => &is_iyr_valid?/1,
      "eyr" => &is_eyr_valid?/1,
      "hgt" => &is_hgt_valid?/1,
      "hcl" => &is_hcl_valid?/1,
      "ecl" => &is_ecl_valid?/1,
      "pid" => &is_pid_valid?/1,
    }
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

  def is_pass_valid_2?(the_pass, required_fields) do
    checks = required_checks()
    required_fields
    |> Enum.all?(fn req_field ->
      {:ok, the_fun} = Map.fetch(checks, req_field)
      if Map.has_key?(the_pass, req_field) do
        {:ok, val} = Map.fetch(the_pass, req_field)
        check_result = the_fun.(val)
        check_result
      else
        false
      end
    end)
  end

  def with_second_check(first_result, list_of_passes) do
    second_result = list_of_passes
    |> Enum.count(fn one_pass ->
      one_pass
      |> is_pass_valid_2?(required_fields())
    end)

    {first_result, second_result}
  end


  def count_valid_passes(list_of_passes) do
    list_of_passes
    |> Enum.count(fn one_pass ->
      one_pass
      |> is_pass_valid?(required_fields())
    end)
    |> with_second_check(list_of_passes)
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
