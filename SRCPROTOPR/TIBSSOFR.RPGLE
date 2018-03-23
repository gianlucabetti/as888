000100120217      /IF DEFINED(DFTACTGRP_YES)
000200120217     D*--------------------------------------------------
000300120220     D* Procedure name: TibsSof_Lettura
000400120217     D* Purpose:        Legge le società delle filiali.
000500120217     D* Returns:
000600120217     D*--------------------------------------------------
000700120220     D TibsSof_Lettura...
000800120217     D                 PR                  EXTPGM('TIBSSOFR')
000900120217     D prmRqsOpCode...
001000120220     D                               10I 0
001100120217     D prmRpyOpCode...
001200120220     D                               10I 0
001300120217     D prmRpyIdMsg...
001400120217     D                               10I 0
001500120217     D prmRqsFormato...
001600120217     D                               10A
001700120217     D prmRqsData...
001800120217     D                            32767A   OPTIONS(*VARSIZE)
001900120217     D prmRqsDataSize...
002000120217     D                               10I 0
002100120217     D prmRpyFormato...
002200120217     D                               10A
002300120217     D prmRpyData...
002400120217     D                            32767A   OPTIONS(*VARSIZE)
002500120217     D prmRpyDataSize...
002600120217     D                               10I 0
002700120217      /ENDIF
002800120217
002900120217      /IF DEFINED(DFTACTGRP_NO)
003000120217
003100120217     D*--------------------------------------------------
003200120220     D* Procedure name: TibsSof_Init
003300120216     D* Purpose:        Inizializza il modulo.
003400120216     D* Returns:        Esito.
003500120216     D*--------------------------------------------------
003600120220     D TibsSof_Init    PR            10I 0
003700120216
003800120216     D*--------------------------------------------------
003900120220     D* Procedure name: TibsSof_Finalize
004000120216     D* Purpose:        Chiude il modulo.
004100120216     D* Returns:
004200120216     D*--------------------------------------------------
004300120220     D TibsSof_Finalize...
004400120220     D                 PR
004500120216
004600120216     D*--------------------------------------------------
004700120220     D* Procedure name: TibsSof_GetIdSocietaByIdFiliale
004800120216     D* Purpose:        Restituisce la società di una filiale alla data.
004900120216     D* Returns:        ID società (*BLANK = errore).
005000120216     D* Parameter:      priIdFiliale => ID filiale.
005100120216     D* Parameter:      priDataRiferimento => Data riferimento per ricerca ...
005200120216     D*                          (*NOPASS = data corrente).
005300120216     D* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società operativa).
005400120216     D*--------------------------------------------------
005500120220     D TibsSof_GetIdSocietaByIdFiliale...
005600120216     D                 PR             3A
005700120216     D  priIdFiliale                  3P 0 CONST                                1
005800120216     D  priDataRiferimento...                                                   2
005900120216     D                                 D   DATFMT(*ISO)
006000120216     D                                     CONST
006100120216     D                                     OPTIONS(*NOPASS:*OMIT)
006200120216     D priTipoSocieta                 1A   CONST                                3
006300120517     D                                     OPTIONS(*NOPASS:*OMIT)
006400120517     D priDataValiditaInizio...                                                 4
006500120517     D                                 D   OPTIONS(*NOPASS)
006600120517     D priDataValiditaFine...                                                   5
006700120517     D                                 D   OPTIONS(*NOPASS)
006800120216
006900120217     D*--------------------------------------------------
007000120220     D* Procedure name: TibsSof_GetIdFilialiByIdSocieta
007100120217     D* Purpose:        Restituisce tutte le filiali di una società alla data.
007200120217     D* Returns:
007300120217     D* Parameter:      priIdSocieta => ID società.
007400120217     D* Parameter:      priDataRiferimento => Data riferimento (*NOPASS = d...
007500120217     D*                          ata corrente).
007600120217     D* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società o...
007700120217     D*                          perativa).
007800120217     D*--------------------------------------------------
007900120220     D TibsSof_GetIdFilialiByIdSocieta...
008000120217     D                 PR             5I 0 DIM(999)
008100120217     D  priIdSocieta                  3A   CONST                                1
008200120217     D  priDataRiferimento...                                                   2
008300120217     D                                 D   DATFMT(*ISO)
008400120217     D                                     CONST
008500120217     D                                     OPTIONS(*NOPASS:*OMIT)
008600120217     D priTipoSocieta                 1A   CONST                                3
008700120217     D                                     OPTIONS(*NOPASS)
008800120217
008900120217     D*--------------------------------------------------
009000120220     D* Procedure name: TibsSof_GetOrganigrammaByIdFiliale
009100120217     D* Purpose:        Restituisce l'organigramma di una filiale.
009200120217     D* Returns:        AZORG00F
009300120217     D* Parameter:      priIdFiliale => ID filiale.
009400120217     D* Parameter:      priNomeFormato => Nome formato (*NOPASS = 'AZORG00F').
009500120217     D* Parameter:      priVersioneFormato => Versione formato (*NOPASS = 'A').
009600120217     D*--------------------------------------------------
009700120220     D TibsSof_GetOrganigrammaByIdFiliale...
009800120217     D                 PR          4282A
009900120217     D  priIdFiliale                  3P 0 CONST
010000120217     D  priNomeFormato...
010100120220     D                               10I 0 CONST
010200120217     D                                     OPTIONS(*NOPASS)
010300120217     D  priVersioneFormato...
010400120217     D                                1A   CONST
010500120217     D                                     OPTIONS(*NOPASS)
010600120217
010700120217      /ENDIF
