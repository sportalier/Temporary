#=
Multiline comments
=#

# version control
versioninfo()

x=2;
x
println(x)


#= IMPORTANT
array index begins at 1 not 0
=#

# install a package
Pkg.add("Plots")

# impot package, either using or import
import Plots
const pl = Plots # this create an an alias, equivalent to Python "import Plots as pl".
# Declaring it constant may improve the performances.
pl.pyplot()
pl.plot(rand(4,4))

# array
a=zeros(5)
b=ones(5)

# matrix
a = [[1,2,3] [4,5,6]]
a = [1 4; 2 5; 3 6]
a[1:2,:]

# dictionnary
mydict = Dict()
mydict["Toto"]=1

# for loop
# note the ";" at the end of the interval
for i in [1:1:10;]
    println(i)
end

# ODEs
Pkg.add("ODE")

using ODE
f(u,p,t) = 1.01*u
u0=1/2
tspan = (0.0,1.0)
prob = ODEProblem(f,u0,tspan)
sol = solve(prob,ode45(),reltol=1e-8,abstol=1e-8)
#using Plots
pl.plot(sol,linewidth=5,title="Solution to the linear ODE with a thick line",
     xaxis="Time (t)",yaxis="u(t) (in μm)",label="My Thick Line!") # legend=false
pl.plot!(sol.t, t->0.5*exp(1.01t),lw=3,ls=:dash,label="True Solution!")
#=

    ode23: 2nd order adaptive solver with 3rd order error control, using the Bogacki–Shampine coefficients
    ode45: 4th order adaptive solver with 5th order error control, using the Dormand Prince coefficients. Fehlberg and Cash-Karp coefficients are also available.
    ode78: 7th order adaptive solver with 8th order error control, using the Fehlberg coefficients.
    ode23s: 2nd/3rd order adaptive solver for stiff problems, using a modified Rosenbrock triple.
=#
#=
ode23 –> BS3()
ode45/dopri5 –> DP5(), though in most cases Tsit5() is more efficient
ode23s –> Rosenbrock23(), though in most cases Rodas4() is more efficient
ode113 –> VCABM(), though in many cases Vern7() is more efficient
dop853 –> DP8(), though in most cases Vern7() is more efficient
ode15s/vode –> QNDF(), though in many cases CVODE_BDF(), Rodas4() or radau() are more efficient
ode23t –> Trapezoid()
ode23tb –> TRBDF2
lsoda –> lsoda() (requires ]add LSODA; using LSODA)
ode15i –> IDA(), though in many cases Rodas4() can handle the DAE and is significantly more efficient
=#
