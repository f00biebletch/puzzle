%
% Copyright 2011 Kevin McIntire, Gianluca Filippini
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not 
% use this file except in compliance with the License. You may obtain a copy 
% of the License at 
%
%    http://www.apache.org/licenses/LICENSE-2.0 
%
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
%
% Significant portions of this code were originally
% written by Joe Armstrong:
% http://www.erlang.org/examples/examples-2.0.html
%
% @doc General crypto support.
%
% $Id$
%

-module(crypto_util).
%-export([generate_key/1, make_prime/1, is_prime/1, pow/3]).
-compile(export_all).

generate_key() ->
    DHg = gen_DHg(),
    DHp = gen_DHp(),
    DHa = gen_DHa(),
    A = crypto:mod_exp(DHg, DHa, DHp),
    DHb = gen_DHb(),
    Key = crypto:mod_exp(A, DHb, DHp),
    list_to_integer(lists:sublist(integer_to_list(Key),16)).
    
gen_DHa() -> gen_DHb().
gen_DHb() -> crypto:rand_uniform(170141183460469231731687303715884105729,
                     340282366920938463463374607431768211455).

gen_DHp() -> gen_DHg().
gen_DHg() -> make_prime(50).

make_prime(K) when K > 0 ->
    new_seed(),
    N = make(K),
    if N > 3 ->
            MaxTries = N - 3,
            P1 = make_prime(MaxTries, N+1),
            P1;
       true ->
            make_prime(K)
    end.

make_prime(0, _) ->
    exit(impossible);
make_prime(K, P) ->
    case is_prime(P) of
        true  -> P;
        false -> make_prime(K-1, P+1)
    end.


make(N) -> new_seed(), make(N, 0).
     
make(0, D) -> D;
make(N, D) -> make(N-1, D*10 + (random:uniform(10)-1)).

new_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).

is_prime(D) ->
    new_seed(),
    is_prime(D, 100).

is_prime(D, Ntests) ->
    N = length(integer_to_list(D)) -1,
    is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;
is_prime(Ntest, N, Len) ->
    K = random:uniform(Len),
    %% A is a random number less than N 
    A = make(K),
    if 
        A < N ->
            case pow(A,N,N) of
                A -> is_prime(Ntest-1,N,Len);
                _ -> false
            end;
        true ->
            is_prime(Ntest, N, Len)
    end.

pow(A, 1, M) ->
    A rem M;
pow(A, 2, M) ->
    A*A rem M;
pow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = pow(A, B1, M),
    case B2 of
        B1 -> (P*P) rem M;
        _  -> (P*P*A) rem M
    end.

