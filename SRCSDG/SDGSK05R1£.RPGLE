000100160616     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160616     H*PARMS BNDDIR(PJXBND PJCBND)
000300160616     H*PARMS CVTOPT(*DATETIME)
000400160616     H DECEDIT('0,') DATEDIT(*DMY/)
000500160616     F*---------------------------------------------------------------*
000600160616     F*            STAMPA DI SERVIZIO MODELLO 770                     *
000700160616     F*                   RIGHE PERCIPIENTI                           *
000800160616     F*---------------------------------------------------------------*
000900160617     FSDGN7701L IF   E           K DISK
001000160616     F*------------
001100160616     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN39)
001200160616     D*---------------------------------------------------------------*
001300160616     D KPJBA         E DS
001400160616     D*------------
001500160617     D ndcr70ds      E DS
001900160616     D*------------
001901160617     D* Dati di ambiente ottenuti da XSOC
001902160617     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002000160616     D* DS Interna per dati di output di XSOC
002100160616     D XSOCDS          S           1000
002200160616     D*-------------
002300160616     D* Reperimento nome PGM
002400160616     D STATUS         SDS           333
002500160616     D  DSPGM            *PROC
002600160616     D  PARMS            *PARMS
002700160616     D*------------
002800160616     D SK              S             66    DIM(2) CTDATA PERRCD(1)              STAMPA
002900160616     D*------------
003000160616     D DatMin          C                   CONST('0001-01-01')
003100160617     D ZDateISO        S               D
003200160616     C*---------------------------------------------------------------*
003300160616     C     *ENTRY        PLIST                                                  *
003400160616     C                   PARM                    KPJBA                          *
003500160616     C*
003600160617     C                   MOVEL     KPJBU         ndcr70ds                       *
003700160616     C*
003800160616     C     KEY770        KLIST                                                  *
003900160616     C                   KFLD                    XSCSOC
004000160616     C                   KFLD                    N77ACF
004100160616     C*
004200160616     C     KEY771        KLIST                                                  *
004300160616     C                   KFLD                    XSCSOC                         *
004400160616     C                   KFLD                    N77ACF                         *
004500160616     C                   KFLD                    N77QUA                         *
004600160616     C*
004700160616     C* Chiamata a xsoc
004800160617     C                   move      socR70        SOCXSC
004900160616    >C                   MOVEL     'SOC001'      TIPXSC
005001160617     C                   CALL      'SDGSOCDR'
005100160616     C                   PARM                    TIPXSC            6
005200160616     C                   PARM                    SOCXSC            3
005300160616     C                   PARM                    CDSXSC            9 0
005400160616     C                   PARM                    MODXSC            3
005500160616     C                   PARM      *BLANKS       RTNXSC            1
005600160616     C                   PARM                    XSOCDS
005700160616     C                   PARM                    KPJBA
005800160616   1 C     RTNXSC        IFNE      '1'
005900160616    >C                   MOVEL     XSOCDS        SOC001
006000160616   1xC                   ELSE
006100160616     C                   GOTO      FINE
006200160616   1-C                   ENDIF
006300160616     C*
006400160616     C* Valorizzazione campi univoci testate
006500160616     C                   MOVEL     XSCDSI        NOMDIT           20
006600160616     C                   MOVEL     DSPGM         NOMPGM           10
006700160616     C* Reperisco nome sistema informatico
006800160616     C                   CALL      'XNETA'                              21
006900160616     C                   PARM                    NOMSYS            8
007000160616     C*
007100160616     C                   seton                                        39
007200160616     C*----------------------------------------------------*
007300160617     C                   z-add     annoR70       n77acf
007400160617     C     KEY770        SETLL     SDGN7701L                                    *
007500160617     C     KEY770        READE     SDGN7701L                              90    *
007600160616     C   90              GOTO      FINE                                         *
007700160616     C*
007701160617     C     flagDR70      IFEQ      *on                                          *
007900160617     C                   MOVE      'SY'          N77QUA                         *
008000160617     C     KEY771        SETLL     SDGN7701L                                    *
008100160617     C     KEY771        READE     SDGN7701L                              90    *
008200160616     C  N90              EXSR      ELAB                                         *
008201160617     C                   ENDIF
008400160616     C*
008500160616     C* Stampo totali generali
008600160616     C     *in39         IFEQ      *on                                          *
008700160616     C                   time                    ORA
008800160616     C                   EXCEPT    TESTA                                        *
008900160616     C                   setoff                                       39        *
009000160616     C                   ENDIF                                                  *
009100160616     C                   EXCEPT    TOTALE                                       *
009200160616     C*
009300160616     C     FINE          TAG                                                    *
009400160616     C                   SETON                                        LR        *
009500160616     C*
009600160616     C*----------------------------------------------------*
009700160616     C*  ELABORAZIONE DETTAGLIO                            *
009800160616     C*----------------------------------------------------*
009900160616     C     ELAB          BEGSR                                                  *
010000160616     C*
010100160616     C                   Z-ADD     0             QUAITC           15 2          *
010200160616     C                   Z-ADD     0             QUAIMN           15 2          *
010300160616     C                   Z-ADD     0             QUASNS           15 2          *
010400160616     C                   Z-ADD     0             QUAIMS           15 2          *
010500160616     C                   Z-ADD     0             QUARAC           15 2          *
010600160617     C                   Z-ADD     0             QUARIM           15 2          *
010700160616     C                   Z-ADD     0             QUANET           15 2          *
011400160616     C                   MOVE      N77QUA        SAVQUA            2            *
012700160616     C*
012800160616     C                   DO        *HIVAL                                       *
012900160616     C                   MOVEL     N77COG        COG                            *
013000160616     C                   MOVEL     N77NOM        NOM                            *
013200160616     C                   MOVEL     N77CIT        CIT                            *
013400160617     C                   MOVEL     N77C77        C77                            *
013600160616     C                   MOVEL     N77VIA        VIA                            *
013700160616     C     N77DNC        IFNE      DatMin
013800160616     C                   CALLb     'XDT4000'
013900160616     C                   PARM      N77DNC        XDTAMG           10
014000160616     C     DNC           PARM                    XDTGMA            6 0
014100160616     C                   PARM      2             XDTSTA            1 0
014200160616     C                   ELSE
014300160616     C                   Z-ADD     0             DNC
014400160616     C                   ENDIF
014600160617     C                   MOVEL     N77CSE        CSE
014700160616     C                   Z-ADD     N77ITC        ITC              15 2
014800160616     C                   Z-ADD     N77IMN        IMN              15 2
014900160616     C                   Z-ADD     N77SNS        SNS              15 2
015000160616     C                   Z-ADD     N77IMS        IMS              15 2
015100160616     C                   Z-ADD     N77RAC        RAC              15 2
015200160617     C                   Z-ADD     N77RIM        RIM              15 2
015300160616     C                   Z-ADD     N77NET        NET              15 2
018400160617     C*
018500160617     C                   Clear                   IndEst           74
018600160617     C                   Movel     N77CIt        IndEst
018700160617     C                   Cat       N77Via:1      IndEst
018800160616     C*
018900160616     C* Totalizzo movimenti parziali per quadro
019000160616     C                   ADD       N77ITC        QUAITC                         *
019100160616     C                   ADD       N77IMN        QUAIMN                         *
019200160616     C                   ADD       N77SNS        QUASNS                         *
019300160616     C                   ADD       N77IMS        QUAIMS                         *
019400160616     C                   ADD       N77RAC        QUARAC                         *
019500160617     C                   ADD       N77RIM        QUARIM                         *
019600160616     C                   ADD       N77NET        QUANET                         *
021500160616     C*
021600160616     C* Sommo nei totali generali
021700160616     C                   ADD       N77ITC        TOTITC           15 2          *
021800160616     C                   ADD       N77IMN        TOTIMN           15 2          *
021900160616     C                   ADD       N77SNS        TOTSNS           15 2          *
022000160616     C                   ADD       N77IMS        TOTIMS           15 2          *
022100160616     C                   ADD       N77RAC        TOTRAC           15 2          *
022200160617     C                   ADD       N77RIM        TOTRIM           15 2          *
022300160616     C                   ADD       N77NET        TOTNET           15 2          *
024200160616     C*
024300160616     C* Stampa dettaglio
024400160616     C     *in39         IFEQ      *on                                          *
024500160616     C                   time                    ORA               6 0
024600160616     C                   EXCEPT    TESTA                                        *
024700160616     C                   setoff                                       39        *
024800160616     C                   ENDIF                                                  *
024801160617     C                   EXCEPT    DETPER
024802160617     C                   If        ITC < 0 or
024803160617     C                             IMN < 0 or
024804160617     C                             SNS < 0 or
024805160617     C                             IMS < 0 or
024806160617     C                             RAC < 0 or
024807160617     C                             RIM < 0 or
024808160617     C                             NET < 0
024809160617     C                   EXCEPT    DETNEG
024810160617     C                   Endif
024900160616     C                   EXCEPT    DET                                          *
025000160616     C*
025100160616     C                   MOVEL     *BLANK        COG              22            *
025200160616     C                   MOVEL     *BLANK        NOM              17            *
025300160616     C                   MOVEL     *BLANK        LNC              20            *
025400160616     C                   MOVEL     *BLANK        CIT              18            *
025700160617     C                   MOVEL     *BLANK        C77               2            *
025900160616     C                   MOVEL     *BLANK        VIA              20            *
026000160616     C                   Z-ADD     0             DNC               6 0          *
026100160616     C                   MOVEL     *BLANK        CSE               4            *
026200160616     C*
026300160617     C     KEY771        READE     SDGN7701L                              90    *
026400160616     C  N90              ENDDO                                                  *
026500160616     C*
026600160616     C* Stampo totali parziali per quadro
026700160616     C     *in39         IFEQ      *on                                          *
026800160616     C                   time                    ORA
026900160616     C                   EXCEPT    TESTA                                        *
027000160616     C                   setoff                                       39        *
027100160616     C                   ENDIF                                                  *
027200160616     C                   EXCEPT    TOTQUA                                       *
027300160616     C*
027400160616     C                   ENDSR                                                  *
027500160616      *----------------------------------------------------
027600160616      * routine iniziale
027700160616      *----------------------------------------------------
027800160616     C     *INZSR        Begsr
027900160617     C     *DMY          Move      Udate         ZDateISO
028000160617     C     *JobRun       Move      ZDateISO      ZDate             6 0
028100160617      *
028200160617     C                   Endsr
028300160617     O*----------------------------------------------------*
028301160617     O* Trasformati i campi importi da edtcde'2'a edcde'K'
028302160617     O*----------------------------------------------------*
028400160617     OQSYSPRT   E            TESTA            02
028500160616     O                       NOMDIT              21
028600160616     O                                           48 '***-------------------'
028700160616     O                                           70 '----------------------'
028800160616     O                                           92 '----------------------'
028900160616     O                                           96 '-***'
029000160616     O                       KNMUS              110
029200160617     O                       ZDATE         Y    120
029300160616     O                                          126 'Pag.'
029400160616     O                       PAGE          Z    131
029500160616     O          E            TESTA       1
029600160616     O                       NOMSYS               9
029700160616     O                       KNSIF               20
029800160616     O                                           27 '*'
029900160616     O                                           53 'Stampa di servizio rig'
030000160617     O                                           75 'he percipienti esteri '
030100160617     O                                           93 'Mod. 770 quadro SY'
030200160616     O                                           96 '*'
030300160616     O                       NOMPGM             110
030400160616     O                       ORA                120 '0 :  :  '
030500160616     O          E            TESTA       1
030600160616     O                                           48 '***-------------------'
030700160616     O                                           70 '----------------------'
030800160616     O                                           92 '----------------------'
030900160616     O                                           96 '-***'
031000160616     O          E            TESTA       2
031100160616     O                       SK(1)               66
031200160616     O                       SK(2)              132
031300160616     O          E            TESTA       1
031400160616     O                                           14 'Codice fiscale'
031500160616     O                                           28 'Cognome'
031600160616     O                                           48 'Nome'
031700160616     O                                           86 'Data / Luogo / Prov. di'
031800160616     O                                           94 'nascita'
031900160616     O                                           98 'Ses'
032000160617     O                                          120 'Comune / Pr di resid.'
032200160617     O                                          126 'Caus.'
032300160617     O                                          132 'Esc.'
032400160617     O          E            TESTA       1
032500160616     O                                            3 'Via'
032700160616     O                                           40 'Importo totale'
032800160616     O                                           56 'Impon.non sogg.'
032900160616     O                                           72 'Spese non sogg.'
033000160616     O                                           88 'Impon.soggetto'
033100160616     O                                           94 '% RA'
033200160616     O                                          111 'Ritenuta'
033300160617     O                                          131 'Netto corrisposto'
033400160617     O*         E            TESTA       1
033500160617     O*                                          12 'Addizionale:'
033600160617     O*                                          25 'Cod.Regione'
033700160617     O*                                          82 'Add.Reg.Acc.'
033800160617     O*                                         107 'Add.Reg.Imp.'
033900160617     O*                                         129 'Add.Reg.Sos.'
034000160617     O*         E            TESTA       1
034100160617     O*                                          25 'Cod.Comune '
034200160617     O*                                          82 'Add.Com.Acc.'
034300160617     O*                                         107 'Add.Com.Imp.'
034400160617     O*                                         129 'Add.Com.Sos.'
034500160617     O          E            TESTA       1
034700160617     O                                           25 'Codice id.fiscale estero '
034800160617     O                                           38 'Stato estero'
034900160617     O                                           84 'Localita / Indirizzo ester'
035000160617     O                                           85 'o'
035100160617     O*         E            TESTA       1
035200160617     O*                                          26 'Redditi da altri soggetti:'
035300160617     O*                                          40 'Cod.Fiscale'
035500160617     O*                                          56 ' Causale   '
035600160617     O*                                          80 'Imponibile'
035700160617     O*                                         107 'Rit.Tit.Acc.'
035800160617     O*                                         129 'Rit.Tit.Imp.'
035900160617     O*         E            TESTA       1
036000160617     O*                                          56 'Rit.Sospese'
036100160617     O*                                          82 'Add.Reg.Acc.'
036200160617     O*                                         107 'Add.Reg.Imp.'
036300160617     O*                                         129 'Add.Reg.Sos.'
036400160617     O*         E            TESTA       1
036500160617     O*                                          82 'Add.Com.Acc.'
036600160617     O*                                         107 'Add.Com.Imp.'
036700160617     O*                                         129 'Add.Com.Sos.'
036800160617     O          E            TESTA       1
036900160617     O                       SK(1)               66
037000160617     O                       SK(2)              132
037100160617     O*
037201160617     O          E            DETPER      1
037300160616     O                       N77CDF              20
037400160616     O                       COG                 43
037500160616     O                       NOM                 61
037600160616     O                       DNC           Y     70
037700160616     O                       LNC                 91
037800160617     O*                      N77Pnc              94
037900160616     O                       N77SES              97
038000160616     O                       CIT                117
038100160617     O*                      N77PRV             120
038300160617     O                       C77                124
038400160617     O                       N77Escludi         130
038401160617     O          E            DETNEG      1
038402160617     O                                        +   1 '** Attenzione, sono stati -
038403160617     O                                              rilevati importi negativi *-
038404160617     O                                              *'
038500160617     O          E            DET         1
038600160617     O                       VIA                 20
038900160617     O                       RIM           K    131
039000160616     O                       RAC           K    111
039100160616     O                       N77PRA        2     95
039200160616     O                       IMS           K     88
039300160616     O                       SNS           K     72
039400160616     O                       IMN           K     56
039500160616     O                       ITC           K     40
039700160617     O*         E            DET         1
039800160617     O*                      N77REG              29
039900160617     O*                      N77ARS        K    131
040000160617     O*                      N77ARI        K    108
040100160617     O*                      N77ARG        K     83
040200160617     O*         E            DET         1
040300160617     O*                      N77COM              29
040400160617     O*                      N77ACS        K    131
040500160617     O*                      N77ACG        K    108
040600160617     O*                      N77ACTA       K     83
040700160617     O          E            DET         1
041100160617     O                       IndESt             132
041200160617     O                       CSE                 30
041300160617     O                       N77CDE              25
041400160617     O*         E    78      DET         1
041500160617     O*                      SCDF                39
041600160617     O*                      SRIM          K    131
041700160617     O*                      SRAC          K    108
041800160617     O*                      SIMS          K     83
042000160617     O*                      SCAU                59
042100160617     O*         E    78      DET         1
042200160617     O*                      SARS          K    131
042300160617     O*                      SARI          K    108
042400160617     O*                      SARA          K     83
042500160617     O*                      SRSO          K     59
042600160617     O*         E    78      DET         1
042700160617     O*                      SACS          K    131
042800160617     O*                      SACI          K    108
042900160617     O*                      SACA          K     83
043000160617     O          E            DET         1
043100160616     O                                            1 ' '
043200160616     O*
043300160616     O          E            TOTQUA      2
043400160616     O                                           13 'Totale quadro'
043500160617     O                       SAVQUA              16
043700160617     O                       QUARIM        K    131
043800160617     O                       QUARAC        K    111
043900160617     O                       QUAIMS        K     88
044000160617     O                       QUASNS        K     72
044100160617     O                       QUAIMN        K     56
044200160617     O                       QUAITC        K     40
044300160617     O          E            TOTQUA      0
044400160617     O                                           13 'Totale quadro'
044500160617     O                       SAVQUA              16
044700160617     O                       QUARIM        K    131
044800160617     O                       QUARAC        K    111
044900160617     O                       QUAIMS        K     88
045000160617     O                       QUASNS        K     72
045100160617     O                       QUAIMN        K     56
045200160617     O                       QUAITC        K     40
045300160617     O*         E            TOTQUA      1
045400160617     O*                      QUAARS        K    131
045500160617     O*                      QUAARI        K    108
045600160617     O*                      QUAARG        K     83
045700160617     O*         E            TOTQUA      1
045800160617     O*                      QUAACS        K    131
045900160617     O*                      QUAACG        K    108
046000160617     O*                      QUAACTA       K     83
046100160617     O*         E            TOTQUA      1
046200160617     O*                                          19 'Tot.Altri Redditi'
046300160617     O*                      QUASRIM       K    131
046400160617     O*                      QUASRAC       K    108
046500160617     O*                      QUASIMS       K     83
046700160617     O*         E            TOTQUA      1
046800160617     O*                      QUASARS       K    131
046900160617     O*                      QUASARI       K    108
047000160617     O*                      QUASARA       K     83
047100160617     O*                      QUASRSO       K     59
047200160617     O*         E            TOTQUA      1
047300160617     O*                      QUASACS       K    131
047400160617     O*                      QUASACI       K    108
047500160617     O*                      QUASACA       K     83
047600160617     O          E            TOTQUA      1
047700160617     O                                            1 ' '
047800160616     O*
047900160616     O          E            TOTALE      2
048000160616     O                                           15 'TOTALE GENERALE'
048100160617     O                       TOTRAC        K    111
048200160617     O                       TOTSNS        K     72
048300160617     O                       TOTITC        K     40
048400160617     O          E            TOTALE      0
048500160617     O                                           15 'TOTALE GENERALE'
048600160617     O                       TOTRAC        K    111
048700160617     O                       TOTSNS        K     72
048800160617     O                       TOTITC        K     40
048900160617     O*
049000160617     O          E            TOTALE      1
049200160617     O                       TOTRIM        K    131
049300160616     O                       TOTIMS        K     88
049400160616     O                       TOTIMN        K     56
049500160617     O          E            TOTALE      0
049700160617     O                       TOTRIM        K    131
049800160617     O                       TOTIMS        K     88
049900160617     O                       TOTIMN        K     56
050000160617     O*         E            TOTALE      1
050100160617     O*                      TOTARS        K    131
050200160617     O*                      TOTARI        K    108
050300160617     O*                      TOTARG        K     83
050400160617     O*         E            TOTALE      1
050500160617     O*                      TOTACS        K    131
050600160617     O*                      TOTACG        K    108
050700160617     O*                      TOTACTA       K     83
050800160617     O*         E            TOTALE      1
050900160617     O*                      TOTSRIM       K    131
051000160617     O*                      TOTSRAC       K    108
051100160617     O*                      TOTSIMS       K     83
051300160617     O*         E            TOTALE      1
051400160617     O*                      TOTSARS       K    131
051500160617     O*                      TOTSARI       K    108
051600160617     O*                      TOTSARA       K     83
051700160617     O*                      TOTSRSO       K     59
051800160617     O*         E            TOTALE      1
051900160617     O*                      TOTSACS       K    131
052000160617     O*                      TOTSACI       K    108
052100160617     O*                      TOTSACA       K     83
052200160617     O          E            TOTALE      1
052300160617     O                                            1 ' '
052400160616**
052500160616------------------------------------------------------------------
052600160616------------------------------------------------------------------
