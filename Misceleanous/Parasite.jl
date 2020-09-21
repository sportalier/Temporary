#=
Test for parasite speed
=#

#using ODE
#Pkg.add("DifferentialEquations")

using DifferentialEquations;
using Distributed;
using SharedArrays;
using DelimitedFiles;

@everywhere function f(u,p,t)
    a1=p[1];
	b1=p[2];
	a2=p[3];
	b2=p[4];
	alpha1=p[5];
	alpha2=p[6];
	upsilon1=p[7];
	upsilon2=p[8];
	gamma1=p[9];
	gamma2=p[10];
	beta11=p[11];
	beta12=p[12];
	beta22=p[13];
	beta21=p[14];
	K1=p[15];
	K2=p[16];

	car1=u[1]+u[2]+u[3];
	car2=u[4]+u[5]+u[6];
	Ntot=car1+car2;

	ydot=[0.0,0.0,0.0,0.0,0.0,0.0,0.0];
	ydot[1]=(a1*u[1]+a1*u[2]+a1*u[3])*(1-car1/K1)+gamma1*u[3]-beta11*u[1]*u[2]/Ntot-beta21*u[1]*u[5]/Ntot;
	ydot[2]=beta11*u[1]*u[2]/Ntot+beta21*u[1]*u[5]/Ntot-upsilon1*u[2]-alpha1*u[2]-b1*u[2];
	ydot[3]=upsilon1*u[2]-gamma1*u[3]-b1*u[3];
	ydot[4]=(a2*u[4]+a2*u[5]+a2*u[6])*(1-car2/K2)+gamma2*u[6]-beta22*u[4]*u[5]/Ntot-beta12*u[4]*u[2]/Ntot;
	ydot[5]=beta22*u[4]*u[5]/Ntot+beta12*u[4]*u[2]/Ntot-upsilon2*u[5]-alpha2*u[5]-b2*u[5];
	ydot[6]=upsilon2*u[5]-gamma2*u[6]-b2*u[6];
	resum=abs(ydot[1])+abs(ydot[2])+abs(ydot[3])+abs(ydot[4])+abs(ydot[5])+abs(ydot[6]);
	if resum<1e-5
		ydot[7]=-11.0;
	else
		ydot[7]=0.0;
	end

	return ydot
end

# @everywhere function condition(u,t,integrator,p)
# 	ydot=f(u,p,t);
# 	resum=sum(ydot);
# 	if resum<1e-5
# 		res=0.0;
# 	else
# 		res=100.0;
# 	end
# 	return res
# end

p=[0.5,0.5,0.02,0.02,0.0,0.0,1.0,0.0,0.2,0.2,0.0,0.6,0.0,0.6,6000.0,4000.0];

param=[0.0,0.2,0.4,0.6,0.8,1.0];
#paramarray=Array{Float64,36,2};
paramarray=zeros(216,3);

index=1;
for ups2=1:6
	upsilon2=param[ups2];
	for gam1=1:6
		gamma1=param[gam1];
		for gam2=1:6
			gamma2=param[gam2];
			paramarray[index,1]=upsilon2;
			paramarray[index,2]=gamma1;
			paramarray[index,3]=gamma2;
			global index+=1;
		end
	end
end

parambeta=zeros(1296,4);
index=1;
for bet11=1:6
	beta11=param[bet11];
	for bet21=1:6
		beta21=param[bet21];
		for bet12=1:6
			beta12=param[bet12];
			for bet22=1:6
				beta22=param[bet22];
				parambeta[index,1]=beta11;
				parambeta[index,2]=beta21;
				parambeta[index,3]=beta12;
				parambeta[index,4]=beta22;
				global index+=1;
			end
		end
	end
end

# for ups2=1:6
# 	upsilon2=param[ups2];
# 	for gam1=1:6
# 		gamma1=param[gam1];
# 		for gam2=1:6
# 			gamma2=param[gam2];
# 			for bet11=1:6
# 				beta11=param[bet11];
# 				for bet21=1:6
# 					beta21=param[bet21];
# 					for bet22=1:6
# 						beta22=param[bet22];
# 						for bet12=1:6
# 							beta12=param[bet12];
# 							paramarray[index,1]=upsilon2;
# 							paramarray[index,2]=gamma1;
# 							paramarray[index,3]=gamma2;
# 							paramarray[index,4]=beta11;
# 							paramarray[index,5]=beta21;
# 							paramarray[index,6]=beta12;
# 							paramarray[index,7]=beta22;
# 							global index+=1;
# 						end
# 					end
# 				end
# 			end
# 		end
# 	end
# end


results=SharedArray{Float64}(3079296,7);
parameterarray=SharedArray{Float64}(3079296,16);
#results=Array{Any}(undef,36);

#@time begin

@sync @distributed for i=1:216
	p[8]=paramarray[i,1];
	p[9]=paramarray[i,2];
	p[10]=paramarray[i,3];
	#p[11]=paramarray[i,4];
	#p[12]=paramarray[i,5];
	#p[13]=paramarray[i,6];
	#p[14]=paramarray[i,7];
	for j=1:1296
		p[11]=parambeta[j,1];
		p[12]=parambeta[j,2];
		p[13]=parambeta[j,3];
		p[14]=parambeta[j,4];

		Ktot=10000;
		kval=[0:10:100;];
		K1par=Ktot*kval/100;
		Ktot2=ones(11)*Ktot;
		K2par=Ktot2-K1par;
		K1par[1]=1.0;
		K2par[end]=1.0;

		for kindex=1:11
			p[15]=K1par[kindex];
			p[16]=K2par[kindex];
			tspan=(0.0,6000.0);
			S01=200.0;
			I01=10.0;
			R01=10.0;
			S02=200.0;
			I02=10.0;
			R02=10.0;
			if kindex==1
				S01=0.0;
				I01=0.0;
				R01=0.0;
			end
			if kindex==11
				S02=0.0;
				I02=0.0;
				R02=0.0;
			end
			u0=[S01,I01,R01,S02,I02,R02,10.0];
			tspan=(0.0,6000.0);
			condition(u,t,integrator) = u[7];
			affect!(integrator) = terminate!(integrator);
			cb = ContinuousCallback(condition,affect!);
			prob = ODEProblem(f,u0,tspan,p);
			sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8,callback=cb);
			final=sol.u[end];
			resu=zeros(1,7);
			for w=1:6
				resu[1,w]=round(final[w],digits=2);
			end
			resu[1,7]=sol.t[end];
			resindex=(i-1)*1296*11+(j-1)*11+kindex;
			#resu2=hcat(p',resu);
			results[resindex,:]=resu;
			parameterarray[resindex,:]=p';
			#loopres[kindex,:]=resu;
		end
	end
	#results[i]=loopres;
end
#end

writedlm("FrequencyDep.6.txt",results);
writedlm("ParamFrequencyDep.6.txt",parameterarray);


#sol = solve(prob,ode45(),reltol=1e-8,abstol=1e-8)


# import Plots
# const pl = Plots
#
# pl.plot(sol)
