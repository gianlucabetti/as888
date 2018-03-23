000100000616     H*PARMS BNDDIR(PKXBND) DFTACTGRP(*NO) ACTGRP(PROBAS)
000200991013     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300011011>>>>>H*PARMS USRPRF(*OWNER)
000400991013      *----------------------------------------------------*
000500000711     H DECEDIT('0,') DATEDIT(*DMY.)
000600991013      *
000700991013      *    Impostazione della lista delle librerie per S.I./lingua
000800991013      *                  -------------------
000900991013      *
001000991013      *
001100991013      * DOCUMENTI MICROANALISI:
001200991013      *
001300991013      * NOTE TECNICHE :
001400991013      *
001500991013      * PERSONALIZZAZIONI
001600991013      *
001700991013      *
001800991013      *
001900991013      *               MAPPA INDICATORI
002000991013      *
002100991013      *  99           INDIC. GENERALE DI ERRORE
002200991013      *
002300991013      *----------------------------------------------------*
002400991013      * Definizione aree di lavoro
002500991013      *----------------------------------------------------*
002600991013      * Parametri
002700991013     d RefParms      E DS                  extname(kplin00f)
002800991013     d SI              S                   like(LinNSI)
002900991013     d RtnCde          S              1A
003000991013     d Lingua          S                   like(LinLingua)
003100991013     D*-------------
003200991013     D* Dati di ambiente ottenuti da BAK500R
003300991013     D SOC001        E DS                  EXTNAME(XSOC001DS)
003400991013     D*-------------
003500991013     D* DS Interna per dati di output di BAK500R
003600991013     D XSOCDS          DS          1000
003700991013      *----------------------------------------------------*
003800991013     D* Reperimento nome PGM
003900991013     D STATUS         SDS           333
004000991013     D  DSPGM            *PROC
004100991013      *
004200991013     C*----------------------------------------------------*
004300991013     C*                MAIN LINE PROGRAM
004400991013     C*----------------------------------------------------*
004500991013      *
004600991013      * Operazioni iniziali
004700991013     c                   exsr      InzVar
004800991013      *
004900991013      * Cambio S.I.
005000991013     c                   call      'BAK002R'                            99
005100991013     c                   parm                    SI
005200991013     c                   parm                    Lingua
005300991013     c                   parm                    RtnCde002         1
005400991013      *
005500991013     c                   if        *in99 or RtnCde002 <> '0'
005600991013     c                   movel     '1'           RtnCde
005700991013     c                   exsr      Endpgm
005800991013     c                   endif
005900991013      *
006000991013      * Fine programma
006100991013     C                   EXSR      EndPgm
006200991013     C/EJECT
006300991013     C************************************************************
006400991013     C* FINE PROGRAMMA
006500991013     C************************************************************
006600991013     C     ENDPGM        BEGSR
006700991013     C*
006800991013     C                   SETON                                            RT
006900991013     C                   RETURN
007000991013     C*
007100991013     C                   ENDSR
007200991013     C/EJECT
007300991013     C************************************************************
007400991013     C* OPERAZIONI INIZIALI
007500991013     C************************************************************
007600991013     C     InzVar        BEGSR
007700991013     C*
007800991013     C* Reperimento parametri
007900991013     C     *ENTRY        PLIST
008000991013     c                   parm                    SI
008100991013     c                   parm                    RtnCde
008200991013      *
008300991117     c                   move      *blanks       RtnCde
008400991013     c                   move      *blanks       Lingua
008500991013      *
008600991013      * Reperimento della lingua attraverso i dati di società
008700991013     C                   exsr      RepSoc
008800991013     C                   IF        XSOCDS <> *BLANKS
008900991013     C                   MOVEL     XSOCDS        SOC001
009000991013     C                   move      XscLin        Lingua
009100991013     C                   ENDIF
009200991013      *
009300991013     C                   ENDSR
009400991013     C/EJECT
009500991013     C************************************************************
009600991013     C* REPERIMENTO DATI SOCIETA'
009700991013     C************************************************************
009800991013     C     REPSOC        BEGSR
009900991013     C*
010000991013     C                   CLEAR                   XSOCDS
010100991013     C                   CALL      'BAK500R'
010200991013     C                   PARM                    XSOCDS
010300991013     C*
010400991013     C                   ENDSR
010500991013     C/EJECT
