002000900413             PGM
002100011023             ENDSBS     SBS(GAICMZ) DELAY(30)
003300011023             DLYJOB     DLY(120)
003500011023             DLTPGM     PGM(GAICMZ/CMZ213C)
003600011023             MOVOBJ     OBJ(EDPSED/CMZ213C) OBJTYPE(*PGM) TOLIB(GAICMZ)
003601011023             STRSBS GAICMZ
017000900507             ENDPGM
