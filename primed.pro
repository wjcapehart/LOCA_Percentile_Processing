

n_primes   = 5000
prime_vals = PRIMES(n_primes)

x = 20454
; x = 34333



for i = 0, n_primes-1 do begin

  if (( x mod  prime_vals(i))  eq 0) then begin
    print, x, prime_vals(i), x/prime_vals(i)
  endif


endfor





end
