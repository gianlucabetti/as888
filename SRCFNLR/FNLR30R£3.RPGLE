000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941017     H* FNLR30R *----------------------------------------------------*
000300920813     H*         - FILTRO LANCIO I.M.A.  E I.M.G.
000400000000     H*--------------------------------------------------------------*
000500941017     FFNLR30D   CF   E             WORKSTN
000600941017     FFNFVV01L  IF   E           K DISK
000700040109     Fazorg01l  IF   E           K DISK
000800941017     D*
000900920813     D TES             S             36    DIM(2) CTDATA PERRCD(1)              TESTATA
001000920813     D TIN             S             12    DIM(2) CTDATA PERRCD(1)              TIPO INV
001100160504     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG DI ERRORE
001200961016     D C78             S              1    DIM(78)                              DI COMODO
001300911015     D*
001400931228     D*** VEDO SE LANCIO IMG O IMA - DATI DA AZIONE
001500931228     D PARAM1          DS
001600931228     D  PARFLG               100    100
001700941017     D*** PASSO DATI A ELABORAZIONE BATCH        - FNLR31R -
001800900523     D PARAM           DS
001900911015     D  VIDNFV                 1      5
002000911015     D  VIDSN                  6      6
002100941028     D* LINEE IN GESTIONE DA CONTROLLARE CON LE SPUNTE IMA
002200920813     D  VIDFIL                 7      9  0
002300941028     D* FILIALE APERTURA FOGLIO
002400941028     D  VIDFLF                10     12  0
002500950810     D* TIPO INVENTARIO
002600950810     D  PARTIN                13     13
002700160502     D* libero
002800160502     D  PAR_lib               14     14
002900060508     D* linea di arrivo
003000060508     D  vidlna                15     17  0
003100070418     D* anche colli mancanti di spedizioni in distinta - solo IMA
003200070418     D  vidmdc                18     18
003300101215     D* solo colli spuntati alla lna finale            - solo IMA
003400101215     D  parimaa               19     19
003500160502     D* TIPO zone consegna
003600160502     D  PARTpm1               20     21
003700160502     D  PARTpm2               22     23
003800941028     D***
003900941017     D*** PARAMETRI PER STAMPA ANOMALIE          - FNLR35R -
004000921016     D PARAM4          DS
004100921016     D  PA4IE                  1      1
004200921016     D  PA4SN                  2      2
004300921016     D  PA4RIS                 3      3
004400921016     D  PA4STA                 4      4
004500921016     D* FASE APERTURA
004600921016     D  PA4FAS                 5      5
004700921016     D* DATE APERTURA
004800941018     D  PA4DAD                11     16  0
004900941018     D  PA4DAA                17     22  0
005000941018     D  CO4DAD                23     28  0
005100941018     D  CO4DAA                29     34  0
005200941018     D* INCLUSIONE ESCLUSIONE ANOMALIE
005300941018     D  PA4INC                35     35
005400941018     D  ANM                   36     65
005500921022     D                                     DIM(10)                              ANOMALIE
005600941018     D* RICHIESTA FOGLIO
005700941018     D  PA4NFV                66     70  0
005800941018     D  PA4DFV                71     76  0
005900950810     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
006000950810     D                 DS
006100950810     D  PA5TLA                 1      1
006200950810     D  PA5NPG                 2      2
006300950810     D  PA5SPG                 3      3
006400950810     D  PA5FAS                 4      4
006500950810     D  PARAM5                 1      4
006600941028     D*
006700941028     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
006800941028     D DSLV50        E DS                  EXTNAME(FNLV50DS)
006900941028     D*
007000920813     D CNCR80        E DS
007100941017     D DSLR01        E DS
007200900517     D KPJBA         E DS
007300921016     D WLBDAT          DS
007400941017     D  G02DAT                 1      8  0
007500941017     D  G02INV                 9     16  0
007600941017     D  G02ERR                17     17
007700941017     D  G02TGI                18     22  0
007800921016     D WGIDAT          DS
007900941017     D  GIODAT                 1      8  0
008000941017     D  GIOINV                 9     16  0
008100941017     D  GIOTGI                17     21  0
008200941018     D                 DS
008300941018     D  WDATA                  1      6  0
008400941018     D  AA                     1      2  0
008500941018     D  GG                     5      6  0
008600040108     D og143         E DS
008700050510     D og150         E DS
008800040108     D DS7N          E DS
008900060508     D FNLV55DS      E DS
009000920813     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009100040108     d wtfaas          S                   LIKE(§ogas)
009200900702     C****************************************************************
009300900702     C*  RIEPILOGO INDICATORI
009400900702     C***************************************************************
009500920813     C* 01    - OFF I.M.A.    ON - I.M.G.
009600050510     C* 02    - Proposta per scelta zona parcel o messaggerie solo per IMA
009700050510     c*         e solo per P.O. abilitati
009800101215     C* 03    - Proposta per scelta solo colli spuntati alla lna finale
009900160502     C* 04    -
010000160502     C* 05    - 2° livello
010100160502     C* 06    - Proposta per scelta zona parcel 1° e/o 2° turno solo per IMA
010200160502     c*         e solo per P.O. abilitati
010300920813     C* 09    - INTERROGAZIONE FOGLI
010400920813     C* 40/50 - ERRORE
010500900702     C* 31/32 - DI COMODO
010600900702     C*****************************************************************
010700000000     C     *ENTRY        PLIST
010800000000     C                   PARM                    KPJBA
010900920813     C                   MOVEL     KPJBU         PARAM1
011000941017     C*
011100000000     C                   Z-ADD     1             CODUT
011200920813     C                   CALL      'X§PARUT'
011300920813     C                   PARM                    UT§DSE
011400920813     C                   MOVEL     REC80         CNCR80
011500941017     C                   MOVEL     RAGUT         RSUT
011600900511     C*---------------------------------------------------------------*
011700920813     C*
011800920813     C                   EXSR      OPERAZ
011900911016     C*
012000900509     C     FOR01         TAG
012100941017     C                   EXFMT     LR30D01
012200941028     C*
012300941028     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
012400941028     C                   SETOFF                                       492851
012500101215     C                   SETOFF                                       444546
012600101215     C                   SETOFF                                       47
012700941017     C*
012800910320     C** CMD3 - FINE LAVORO
012900900509     C   KC              GOTO      FINE
013000941028     C**
013100920813     C** CMD18- CAMBIO FILIALE
013200920813     C   KS              SETON                                        04
013300920813     C   KS              GOTO      FOR01
013400920813     C*
013500060913     c                   if        *in04
013600060913     C                   EXSR      CTRFGS
013700060913     c                   if        not *in90
013800060913     c                   setoff                                       04
013900060913     c                   endif
014000060913     c
014100060913     C                   goto      for01
014200060913     c                   endif
014300920813     C*
014400910801     C*  CONTROLLI
014500910801     C                   EXSR      CONTR
014600941019     C  NKF
014700910801     COR 90              GOTO      FOR01
014800070418     C* F6 - ELABORAZIONE
014900950810     C                   CLEAR                   KPJBU
015000160502     c                   if        not *in01 and (*in02  or *in06)
015100160502     c                   eval      partpm1=vidtpm
015200160502     c                   eval      partpm2=vidtpm2
015300050510     c                   else
015400160502     c                   clear                   partpm1
015500160502     c                   clear                   partpm2
015600050510     c                   endif
015700160502
015800101215     c                   if        not *in01 and *in03
015900101215     c                   eval      parimaa=vidimaa
016000101215     c                   else
016100101215     c                   clear                   parimaa
016200101215     c                   endif
016300900702     C                   MOVEL     PARAM         KPJBU
016400941017     C                   MOVEL     'LR31'        KCOAZ
016500900702     C                   CALL      'BCH10'
016600900702     C                   PARM                    KPJBA
016700921016     C**
016800921016     C* ANOMALIE
016900921016     C                   MOVEL     VIDNFV        PA4NFV                          FOGLIO
017000921016     C*
017100921016     C                   MOVE      FVVDFV        G02INV                          DATA DAL
017200921016     C                   MOVE      *ZEROS        G02DAT
017300921016     C                   MOVEL     3             G02ERR
017400941017     C                   CALL      'XSRDA8'
017500921016     C                   PARM                    WLBDAT
017600950317     C                   MOVEL     G02DAT        PA4DFV
017700950324     C                   MOVE      G02DAT        WAA               2 0
017800950324     C                   MOVE      WAA           PA4DFV
017900921016     C* SOTRAGGO 1 ALLA DATA FOGLIO
018000921016     C     G02TGI        SUB       1             GIOTGI
018100921016     C*
018200941017     C                   CALL      'XSRGI8'
018300921016     C                   PARM                    WGIDAT
018400921016     C*
018500941018     C                   Z-ADD     GIOINV        CO4DAD                          AA/MM/GG
018600941018     C                   Z-ADD     CO4DAD        WDATA                           DI COMODO
018700941018     C                   Z-ADD     CO4DAD        PA4DAD
018800941018     C                   MOVEL     GG            PA4DAD
018900941018     C                   MOVE      AA            PA4DAD                          GG/MM/AA
019000921016     C* SOMMO 1 AI GIORNI ALLA DATA FOGLIO
019100921016     C     G02TGI        ADD       1             GIOTGI                          DATA AL
019200941017     C                   CALL      'XSRGI8'
019300921016     C                   PARM                    WGIDAT
019400921016     C*
019500941018     C                   Z-ADD     GIOINV        CO4DAA                          AA/MM/GG
019600941018     C                   Z-ADD     CO4DAA        WDATA                           DI COMODO
019700941018     C                   Z-ADD     CO4DAA        PA4DAA
019800941018     C                   MOVEL     GG            PA4DAA
019900941018     C                   MOVE      AA            PA4DAA                          GG/MM/AA
020000921016     C*
020100950810     C                   CLEAR                   KPJBU
020200960529     C* IMPOSTO LA KPJBU
020300941028     C                   CLEAR                   DSLR01
020400941028     C                   Z-ADD     3             DLRNPG                          CATEGORIA
020500941028     C                   MOVEL     VIDNFV        DLRNFV                          NUM FOGLIO
020600941028     C                   Z-ADD     *ZERO         DLRNPD                          PISTOLA INIZIALE
020700941028     C                   Z-ADD     *HIVAL        DLRNPA                          PISTOLA FINALE
020800941028     C                   Z-ADD     *ZERO         DLRSRD                          SERIE INIZIALE
020900941028     C                   Z-ADD     *HIVAL        DLRSRA                          SERIE FINALE
021000941028     C                   MOVEL     VIDFLF        DLRFGS                          FIL APERT F
021100941028     C                   MOVEL     'S'           DLRFS1                          STAMPA SPU
021200950810     C                   CLEAR                   KPJBU
021300941028     C                   MOVEL     DSLR01        KPJBU
021400960529     C**
021500960529     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
021600960529     C     VIDSSP        IFEQ      'S'
021700941017     C                   MOVEL     'LR06'        KCOAZ
021800920930     C                   CALL      'BCH10'
021900920930     C                   PARM                    KPJBA
022000920930     C                   END
022100971128     C*
022200971128     C                   MOVEL     'C'           D55TLA
022300971128     C                   CALL      'FNLV55R'
022400060508     C                   PARM                    fnlv55ds
022500910320     C*
022600000000     C     FINE          TAG
022700000000     C                   SETON                                        LR
022800941017     C**************************************************************************
022900941017     C* OPERAZIONI INIZIALI
023000941017     C**************************************************************************
023100920813     C     OPERAZ        BEGSR
023200920813     C*
023300020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
023400920813     C*
023500020424     C                   Z-ADD     SIMPOU        VIDFIL
023600020424     C     SIMTPP        IFEQ      '2'
023700020424     C     SIMTPP        OREQ      *BLANKS
023800020424     C                   SETON                                        05
023900020424     C                   ENDIF
024000020424     C*
024100931228     C                   MOVEL     VIDFIL        VIDFLF
024200931228     C*
024300931228     C* 01 ON - I M G
024400050510    1C     PARFLG        IFEQ      'G'
024500931228     C                   SETON                                        01
024600050510   x1C                   else
024700050510     c* Vedo se p.o. gestione abilitato al piking messaggerie
024800050510     c     vidfil        chain     azorg01l
024900050510    2c                   if        %found(azorg01l)
025000050510     c                   movel     orgdf0        og150
025100050510    3c                   if        §ogpkm='S'
025200050510     c                   seton                                        02
025300050510    3C                   END
025400101215    3c                   if        §ogimaa='S'
025500101215     c                   seton                                        03
025600101215    3C                   END
025700160502     c                   if        §ogturno='S'
025800160502     c                   seton                                        06
025900160502    3C                   END
026000101215     c
026100050510    2C                   END
026200050510    1C                   END
026300931228     C*
026400931228     C* CAMPI DI PASSAGGIO
026500931228     C*
026600931228     C  N01              MOVE      TES(1)        VIDTES
026700931228     C  N01              MOVEL     TIN(1)        VIDTIN
026800931228     C   01              MOVE      TES(2)        VIDTES
026900931228     C   01              MOVEL     TIN(2)        VIDTIN
027000931228     C                   Z-ADD     3             COMNPG
027100950810     C   01              MOVEL     'G'           PA5SPG
027200950810     C  N01              MOVEL     'A'           PA5SPG
027300950810     C                   MOVEL     PA5SPG        PARTIN
027400931228     C*
027500931228     C* PRENDO FASE ANOMALIA
027600931228     C*
027700950810     C                   MOVEL     'L'           PA5TLA
027800950810     C                   MOVEL     3             PA5NPG
027900950810     C                   CLEAR                   PA5FAS
028000950810     C                   CALL      'TRUL11R'
028100950810     C                   PARM                    PARAM5
028200950810     C*
028300950810     C                   MOVEL     PA5FAS        PA4FAS            1
028400931228     C*
028500931228     C*
028600931228     C                   MOVEL     *BLANKS       VIDNFV
028700931228     C                   MOVEL     'N'           VIDSN
028800960430     C                   MOVEL     'N'           VIDSSP
028900060508     c                   clear                   vidlna
029000931228     C* PER ANOMALIE
029100931228     C                   MOVEA     *BLANKS       ANM
029200931228     C                   MOVEL     'I'           PA4IE
029300931228     C                   MOVEL     ' '           PA4SN
029400931228     C                   MOVEL     ' '           PA4RIS
029500931228     C                   MOVEL     'N'           PA4STA
029600931228     C                   MOVEL     'I'           PA4INC
029700070418     C                   MOVEL     'N'           vidmdc
029800110131     C  n03              MOVEL     'N'           vidimaa
029900110131     C   03              MOVEL     'S'           vidimaa
030000941017     C*
030100920813     C                   ENDSR
030200941028      *-----------------------------------------------------***********
030300941028      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
030400941028      *-----------------------------------------------------***********
030500941028     C     CTRFGS        BEGSR
030600941028     C                   SETOFF                                       90
030700941028     C                   CLEAR                   DSLV50
030800941028     C                   MOVEL     KNMUS         D50PRU
030900941028     C                   MOVEL     VIDFIL        D50FGS
031000941028     C                   CALL      'FNLV50R'
031100941028     C                   PARM                    DSLV50
031200941028     C*
031300941028     C     D50ERR        IFNE      ' '
031400941028     C                   MOVEL     D50MSG        V1CMSG
031500941028     C                   SETON                                        499028
031600941028     C                   GOTO      ENDCTF
031700941028     C                   ENDIF
031800050510     c* Verifico se ha piking messaggerie: abilito i disabilito
031900050510     c*  il campo
032000101215     c* Verifico se ha ima per colli spuntati in arrivo: abilito o disabilito
032100160906     c                   setoff                                       020306
032200101215     c
032300050510     c                   if        not *in01
032400050510     c     vidfil        chain     azorg01l
032500050510     c                   if        %found(azorg01l)
032600050510     c                   movel     orgdf0        og150
032700101215     c
032800101215    3c                   if        §ogpkm='S'
032900101215     c                   seton                                        02
033000101215    3C                   END
033100101215     c
033200101215     c                   if        §ogimaa='S'
033300101215     c                   seton                                        03
033400110131     c                   movel     'S'           vidimaa
033500050510     C                   END
033600160906     c                   if        §ogturno='S'
033700160906     c                   seton                                        06
033800160906    3C                   END
033900101215     c
034000050510     C                   END
034100101215     C                   END
034200941028     C*
034300941028     C     ENDCTF        ENDSR
034400941017     C**************************************************************************
034500941017     C* CONTROLLO CAMPI
034600941017     C**************************************************************************
034700910320     C     CONTR         BEGSR
034800941017     C*
034900920624     C                   SETOFF                                       9030
035000941028     C*
035100931228     C* FILIALE DI APETTURA FOGLIO
035200941028     C                   CLEAR                   DSLV50
035300941028     C                   MOVEL     KNMUS         D50PRU
035400941028     C                   MOVEL     VIDFLF        D50FGS
035500941028     C                   CALL      'FNLV50R'
035600941028     C                   PARM                    DSLV50
035700941028     C*
035800941028     C     D50ERR        IFNE      ' '
035900941028     C                   MOVEL     D50MSG        V1CMSG
036000941028     C                   SETON                                        519028
036100941028     C                   GOTO      ENDCT1
036200941028     C                   ENDIF
036300931228     C*
036400050510     C* NUMERO FOGLIO
036500911015     C     VIDNFV        IFEQ      *BLANKS
036600911015     C     VIDNFV        OREQ      *ZEROS
036700911015     C                   SETON                                        4090
036800911015     C                   GOTO      ENDCT1
036900911015     C*
037000911015     C                   ELSE
037100911015     C* RICERCA
037200931228     C     '?'           SCAN      VIDNFV                                 90
037300931228     C   90              DO
037400941017     C                   CLEAR                   DSLR01
037500941017     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
037600941028     C                   MOVEL     'FNLR30R'     DLRPGM                         PROGRAMMA
037700941017     C                   MOVEL     '2'           DLRTFV
037800941017     C                   MOVEL     VIDFLF        DLRFGS
037900941017     C                   MOVEL     3             DLRNPG
038000941017     C                   MOVE      31129999      DLRGAL
038100941017     C                   MOVE      99991231      DLRAAL
038200941017     C                   MOVEL     DSLR01        KPJBU
038300941017     C                   CALL      'FNLR02R'
038400911015     C                   PARM                    KPJBA
038500941017     C                   MOVEL     KPJBU         DSLR01
038600941017     C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
038700941017     C                   MOVEL     DLRNFV        VIDNFV
038800941017     C                   ENDIF
038900911016     C*
039000911015     C                   GOTO      ENDCT1
039100911015     C                   END
039200911016     C*  C O N T R O L L I
039300911015     C                   TESTN                   VIDNFV               31
039400930914     C     *IN31         IFEQ      *OFF
039500930914     C                   SETON                                        30
039600930914     C*
039700930914     C                   ELSE
039800911015     C*
039900930914     C                   MOVEL     VIDNFV        COMNFV
040000941017     C     KFVV          CHAIN     FNFVV01L                           30
040100911015     C* ANNULLATO
040200911015     C  N30FVVATB        IFNE      *BLANKS
040300911015     C                   SETON                                        30
040400911015     C                   END
040500930914     C                   ENDIF
040600911016     C* ERRORE
040700911015     C   30              SETON                                        4090
040800911015     C   30              GOTO      ENDCT1
040900920813     C*
041000911015     C* CHIUSO
041100930914     C     FVVFCF        IFNE      *BLANKS
041200911015     C                   SETON                                        4290
041300911016     C                   GOTO      ENDCT1
041400911015     C                   END
041500911016     C*
041600911016     C* NON E' INVENTARIO ARRIVI
041700950810     C  N01FVVSPG        IFNE      PA5SPG
041800911016     C                   SETON                                        4190
041900911016     C                   GOTO      ENDCT1
042000911016     C                   END
042100920813     C*
042200920813     C* NON E' INVENTARIO GIACENZE
042300950810     C   01FVVSPG        IFNE      PA5SPG
042400920813     C                   SETON                                        4390
042500920813     C                   GOTO      ENDCT1
042600920813     C                   END
042700911015     C*
042800911015     C                   END
042900050510     c
043000050510     c* Per elaborazione definitiva, accetto solo ' ' nella scelta
043100050510     c*  delle zone di consegna
043200160502     c                   if        not *in01 and  vidsn='S'
043300160502     c                             and (*in02 or *in06)
043400160502     c                   if        vidtpm<>'  ' or vidtpm2<>'  '
043500050510     C                   SETON                                        902844
043600050510     C                   MOVEA     MSG(2)        v1cmsg
043700060508     C                   GOTO      ENDCT1
043800050510     C                   END
043900160502     C                   END
044000160502     c*
044100160502     c                   if        not *in01 and not *in06  and *in02
044200160502     c                   if        vidtpm='P1' or vidtpm='P2' or
044300160502     c                             vidtpm2='P1' or vidtpm2='P2'
044400160502     C                   SETON                                        902844
044500160502     C                   MOVEA     MSG(7)        v1cmsg
044600160502     C                   GOTO      ENDCT1
044700160502     c                   endif
044800160502     c                   endif
044900160502     c                   if        not *in01 and *in06  and not *in02
045000160502     c                   if        vidtpm='P ' or vidtpm='M ' or
045100160502     c                             vidtpm2='P ' or vidtpm2='M '
045200160502     C                   SETON                                        902844
045300160502     C                   MOVEA     MSG(7)        v1cmsg
045400160502     C                   GOTO      ENDCT1
045500160502     c                   endif
045600160502     c                   endif
045700160504     c* non posso mai accettare P +P1 o P+P2
045800160504     c                   if        vidtpm='P ' or  vidtpm2='P '
045900160504     c                   if        vidtpm='P1' or vidtpm='P2' or
046000160504     c                             vidtpm2='P1' or vidtpm2='P2'
046100160504     C                   SETON                                        902844
046200160504     C                   MOVEA     MSG(8)        v1cmsg
046300160504     C                   GOTO      ENDCT1
046400160504     c                   endif
046500160504     c                   endif
046600160502
046700060508     c* non posso selezionare linea di arrivo se elaborazione definitiva
046800060508     c                   if        vidsn='S' and vidlna>0
046900060508     C                   SETON                                        902845
047000060508     C                   MOVEA     MSG(3)        v1cmsg
047100060508     C                   GOTO      ENDCT1
047200060508     C                   END
047300101215     c* non posso selezionare solo colli spuntati alla lna finale se elab.definitiva
047400101215     c                   if        vidsn='S' and vidimaa='S'
047500101215     C                   SETON                                        902847
047600101215     C                   MOVEA     MSG(6)        v1cmsg
047700101215     C                   GOTO      ENDCT1
047800101215     C                   END
047900060508     c* Deve essere gestita dal p.o. in gestione
048000060508     c                   if        vidlna>0 and vidlna<>vidfil
048100060508     c                   clear                   fnlv55ds
048200060508     c                   eval      d55tpt='6'
048300060508     c                   eval      d55lin=vidlna
048400060508     c                   eval      d55drf=fvvdfv
048500060508     c                   call      'FNLV55R'
048600060508     c                   parm                    fnlv55ds
048700060508     c
048800060508     c                   if        d55tfa<>vidfil
048900060508     C                   SETON                                        902845
049000060508     C                   MOVEl     MSG(4)        v1cmsg
049100060508     C                   GOTO      ENDCT1
049200060508     c                   endif
049300060508     c                   endif
049400070706     c* Per elaborazione definitiva non posso elanciare in data >
049500070706     c                   if        vidsn='S' and fvvdfv>dateu
049600070706     C                   SETON                                        902846
049700070706     C                   MOVEA     MSG(5)        v1cmsg
049800070706     C                   GOTO      ENDCT1
049900070706     C                   END
050000910320     C*
050100911015     C     ENDCT1        ENDSR
050200941017     C**************************************************************************
050300941017     C*    R O U T I N E     I N I Z I A L E
050400941017     C**************************************************************************
050500941017     C     *INZSR        BEGSR
050600941017     C*
050700941017     C     KFVV          KLIST
050800941017     C                   KFLD                    COMNPG
050900941017     C                   KFLD                    COMNFV
051000941028     C                   KFLD                    VIDFLF
051100941017     C*
051200941017     C     *LIKE         DEFINE    FVVNPG        COMNPG                         CATEGORIA FOGLIO
051300941017     C     *LIKE         DEFINE    FVVNFV        COMNFV                         NR.FOGLIO
051400961017     C     *LIKE         DEFINE    VIDNFV        WNFV
051500971128     C     *LIKE         DEFINE    D55TFA        WFE0
051600971128     C     *LIKE         DEFINE    D55TFP        WTFP
051700070706     c* data del giorno
051800070706     C                   TIME                    W0140            14 0
051900070706     C* UDATE IN GGMMAAAA
052000070706     C                   MOVE      W0140         WDTGIO            8 0
052100070706     C*
052200070706     C* UDATE IN AAAAMMGG
052300070706     C                   Z-ADD     WDTGIO        G02DAT
052400070706     C                   MOVEL     *BLANK        G02ERR
052500070706     C                   CALL      'XSRDA8'
052600070706     C                   PARM                    WLBDAT
052700070706     C                   MOVEL     G02INV        DATEU             8 0
052800941017     C*
052900941017     C                   ENDSR
053000941017     C**************************************************************************
053100961016**
053200911018 ** INVENTARIO  MAGAZZINO  ARRIVI **
053300920813 ** INVENTARIO MAGAZZINO GIACENZE **
053400911018**
053500911018 I . M . A .
053600920813 I . M . G .
053700961016**  MSG
053800101215Manca l'elaboraz. dell'IDA del proprio terminal di arr. xxx. ENTER PER FORZARE     1
053900160504Per elaborazione DEFINITIVA non si può scegliere il tipo zona consegna             2
054000101215Per elaborazione DEFINITIVA non si può selezionare la linea di arrivo              3
054100101215Linea di Arrivo errata: non in gestione al P.O. selezionato                        4
054200101215Impossibile elaborazione definitiva per foglio in data MAGGIORE di oggi            5
054300101215Per elaborazione DEFINITIVA non selezionare solo colli spuntati alla LNA finale    6
054400160502Tipo scelta zone di consegna non prevista                                          7
054500160504Scelta tipi  zone di consegna non corretta                                         8
