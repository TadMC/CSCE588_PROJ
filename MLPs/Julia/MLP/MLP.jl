


function Sigm(X)
	return .5 * ( tanh(X*.5)+1)
end

function dSigm(X)
	return Sigm(X).*(1-Sigm(X))
end

function feedforward(IN, WIN, BIN)
	# IN  : NxM
	# WIN : MxP
	# BIN : 1xP
	# AIN, H : NxP
	AIN = IN * WIN + BIN
	H = Sigm(AIN)
	return AIN, H
end

function reconstruct(H, WOUT, BOUT)

	AOUT = H*WOUT + BOUT
	O = Sigm(AOUT)
	return AOUT, O
end


function calcError(IN, OUT, WOUT, AIN, AOUT)

	ERR = OUT - IN

	H2OERR = ERR .* dSigm(AOUT)
	I2OERR = (H2OERR * WOUT' ).* dSigm(AIN)
	return H2OERR, I2OERR

end 


function UpdateWandB(IN, H, H2O, I2H)

	DBIN = sum(I2H, 1)
	DBOUT = sum(H2O, 1)

	DWIN = IN'*I2H  
	DWOUT = H'* H2O 
	return DBIN, DBOUT, DWIN, DWOUT
end


function sseCost(IN, O)
	# This should always be less than
	# the previous sseCost value
	return sum((IN.-O).*2)
end


function main()


	#IN : NxM
	#WIN : MxP
	#BIN : 1xP
	# HIDD : NxP
	#WOUT : PXM
	#BOUT : 1xM
	#O : NxM

	IN = ones(6,4)
	WIN = rand(4,3)
	BIN = zeros(1,3)
	WOUT = rand(3,4)
	BOUT = zeros(1,4)

	WLC = .0001
	BLC = .0001

    #=================
    LOOP
    =#
	AIN, H = feedforward(IN, WIN, BIN)
	AOUT, O = reconstruct(H, WOUT, BOUT)
	COST = sseCost(IN, O)
	H2O, I2O = calcError(IN, OUT, WOUT, AIN, AOUT)
	DBIN, DBOUT, DWIN, DWOUT = UpdateWandB(IN, H, H2O, I2H)
	
	BIN = BIN + BLC.*DBIN
	BOUT = BOUT + BLC.* DBOUT

	WIN = WIN + WLC.*DWIN
	WOUT = WOUT + WLC.*DWOUT
	#ENDLOOP#




