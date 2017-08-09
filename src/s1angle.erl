-module(s1angle).

-include("s2geo.hrl").

%% API exports
-export([
  new/0,
  new_from_radians/1,
  new_from_degrees/1,
  new_from_e5/1,
  new_from_e6/1,
  new_from_e7/1,
  abs/1
  ]).

new() -> {s1angle, 0.0}.

new_from_radians(Radians) when is_number(Radians) -> {s1angle, float(Radians)}.

new_from_degrees(Degrees) when is_number(Degrees) -> {s1angle, float(Degrees) * (?M_PI / 180.0)}.

new_from_e5(E5) when is_number(E5) -> {s1angle, float(E5) * 1.0e5 }.
new_from_e6(E6) when is_number(E6) -> {s1angle, float(E6) * 1.0e6 }.
new_from_e7(E7) when is_number(E7) -> {s1angle, float(E7) * 1.0e7 }.

abs({s1angle, Radians}) -> {s1angle, erlang:abs(Radians)}.
