000100150511     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200150511     H*PARMS CVTOPT(*NONE)
000300150511     H DECEDIT('0,') DATEDIT(*DMY.)
000400150511      *
000401150514      *
000500160728      * Controllo sovrapposizione periodi a regime agevolato
000602150514      *
000603150514      *
000604161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000605161020R235  *
000700150511     F*------------
000701160728     F* Storico percipienti a regime agevolato
000702160728     F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
000703160728     FSDGSPA01L IF   E           K DISK
000900150511     D*-------------
001000150511     D* Passaggio Parametri
001100150511     D KPJBA         E DS
001200150511     D*-------------
001300160728     D* Passaggio Parametri di ricezione
001400160728     D SDGSK13DS     E DS
001500150511     D*-------------
001600150511     D* Variabili di appoggio
001700150511     D $RcdOk          S              1
001800150511     C*----------------------------------------------------*
001900150511     C*                MAIN LINE PROGRAM
002000150511     C*----------------------------------------------------*
002100150511     C*
002300150514     C                   EXSR      INZVAR
002400150511     C*
002500160728     C* lettura periodi a regime agevolato
002600160728     C                   EXSR      ElaSPA
002700150511     C*
002900150514     C                   EXSR      ENDPGM
003000150511     C*
003100150511      /EJECT
003200150511     C************************************************************
003300150511     C* Fine Programma
003400150511     C************************************************************
003500150511     C     ENDPGM        BEGSR
003600150511     C*
003700150511     C* passaggio dati al pgm chiamante
003800160728     C                   MoveL     SDGSK13DS     KPJBU
003900150511     C*
004000150511     C                   SetOn                                            LR
004100150511     C                   Return
004200150511     C*
004300150511     C                   ENDSR
004400150511      /EJECT
004500150511     C************************************************************
004600160728     C* CICLO DI LETTURA DEI periodi a regime agevolato
004700150511     C************************************************************
004800160728     C     ElaSPA        BEGSR
004900150511     C*
005000150511     C* per default ritorno 'periodo non sovrapposto'
005100160728     C                   Move      *Off          SovraK13
005200150511     C*
005300150511     C* ciclo per tutti i record del conto
005400150511     C* (termino appena trovo un periodo sovrapposto)
005500160728     C                   Movel     SocietaK13    SPASocieta
005600160728     C                   Movel     KccK13        SPAKcc
005601160728     C                   Movel     KscK13        SPAKsc
005800160728     C     K03SPA01      Setll     SDGSPA01L
005900160728     C     K03SPA01      ReadE     SDGSPA01L                              21
006000150511     C*
006100150511     C     *In21         DoWEq     *Off
006200160728     C     SovraK13      AndEq     *Off
006300150511     C*
006400160728     C                   EXSR      SelSPA
006500150511     C     $RcdOk        IfEq      *On
006600160728     C                   EXSR      CtrSPA
006700150511     C                   EndIf
006800150511     C*
006801160728     C     K03SPA01      ReadE     SDGSPA01L                              21
007000150511     C*
007100150511     C                   EndDo
007200150511     C*
007300150511     C                   ENDSR
007400150511      /EJECT
007500150511     C************************************************************
007600160728     C* SELEZIONE periodo a regime agevolato
007700150511     C************************************************************
007800160728     C     SelSPA        BEGSR
007900150511     C*
008000150511     C                   Move      *On           $RcdOk
008100150511     C*
008200150511     C                   Select
008300150511     C*
008400150511     C* se il controllo avviene durante la gestione di un record
008500150511     C* esistente (opzioni di modifica e anche visualizzazione),
008600160728     C* lo devo scartare nel momento in cui lo trovo su SDGSPA:
008700150511     C* altrimenti facendo un confronto con i suoi stessi periodi,
008800150511     C* troverei sempre 'periodo sovrapposto ad uno esistente'!
008900160728     C                   When      (OpzK13 = '02' Or OpzK13 = '05') And
009000160728     C                               SPADtIni = DtIniK13
009100150511     C                   Move      *Off          $RcdOk
009200150511     C*
009300150511     C                   EndSl
009400150511     C*
009500150511     C                   ENDSR
009600150511      /EJECT
009700150511     C************************************************************
009800160728     C* CONTROLLO periodo a regime agevolato
009900150511     C************************************************************
010000160728     C     CtrSPA        BEGSR
010100150511     C*
010200150511     C* se data finale non valorizzata assumo data massima
010300160728     C     SPADtFin      IfEq      *Loval
010400160728     C                   Move      *Hival        SPADtFin
010500150511     C                   EndIf
010600150511     C*
010700150511     C                   Select
010800150511     C*
010900150511     C* il periodo ricevuto comincia in una data compresa nel periodo
011000150511     C* trovato in anagrafica
011100160728     C* (es. ricevo 1/1/16-31/01/16 e leggo 15/12/15-15/01/16)
011200160728     C                   When      DtIniK13 >= SPADtIni And
011300160728     C                             DtIniK13 <= SPADtFin
011400160728     C                   Move      *On           SovraK13
011500150511     C*
011600150511     C* il periodo ricevuto termina in una data compresa nel periodo
011700150511     C* trovato in anagrafica
011800160728     C* (es. ricevo 1/1/01-31/01/01 e leggo 15/01/16-15/02/16)
011900160728     C                   When      DtFinK13 >= SPADtIni And
012000160728     C                             DtFinK13 <= SPADtFin
012100160728     C                   Move      *On           SovraK13
012200150511     C*
012300150511     C* il periodo ricevuto non inizia e non termina all'interno
012400150511     C* del periodo trovato in anagrafica, ma è il periodo trovato
012500150511     C* in anagrafica che comincia e termina nel periodo ricevuto
012600160728     C* (es. ricevo 1/1/01-31/01/01 e leggo 10/01/16-20/01/16)
012700160728     C                   When      SPADtIni >= DtIniK13 And
012800160728     C                             SPADtIni <= DtFinK13 And
012900160728     C                             SPADtFin >= DtIniK13 And
013000160728     C                             SPADtFin <= DtFinK13
013100160728     C                   Move      *On           SovraK13
013200150511     C*
013300150511     C                   EndSl
013400150511     C*
013500150511     C                   ENDSR
013600150511      /EJECT
013700150511     C************************************************************
013800150511     C* INIZIALIZZAZIONE VARIABILI
013900150511     C************************************************************
014000150511     C     INZVAR        BEGSR
014100150511     C*
014200150511     C     *ENTRY        PLIST
014300150511     C                   PARM                    KPJBA
014400150511     C*
014500160728     C                   MOVEL     KPJBU         SDGSK13DS
014600150511     C*
014700150511     C* se data finale non valorizzata assumo data massima
014800160728     C     DtFinK13      IfEq      *Loval
014900160728     C                   Move      *Hival        DtFinK13
015000150511     C                   EndIf
015100150511     C*
015200150511     C                   ENDSR
015300150511      /EJECT
015400150511     C************************************************************
015500150511     C* DEFINIZIONE KLIST
015600150511     C************************************************************
015700150511     C     DefKlist      BEGSR
015800150511     C*
015900150511     C* klist
016000150511     C*
016100160728     C* SDGSPA01L
016200160728     C     K03SPA01      KList
016300160728     C                   KFld                    SPASocieta
016400160728     C                   KFld                    SPAKcc
016500160728     C                   KFld                    SPAKsc
016600150511     C*
016700150511     C                   ENDSR
016800150511      /EJECT
