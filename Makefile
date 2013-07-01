BEAMDIR = ebin

all:
	erl -make

clean:
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crush.dump ;
	@ mkdir -p $(BEAMDIR)
