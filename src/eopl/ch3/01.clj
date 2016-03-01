Let ρ = [i=1,v=5,x=10].

(value-of
  <<-(-(x,3), -(v,i))>>
  ρ)
＝ ⌈(-
     ⌊(value-of <<-(x,3)>> ρ)⌋
     ⌊(value-of <<-(v,i)>> ρ)⌋)⌉