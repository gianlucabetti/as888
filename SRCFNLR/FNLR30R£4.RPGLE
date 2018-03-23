000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941017     H* FNLR30R *----------------------------------------------------*
000300920813     H*         - FILTRO LANCIO I.M.A.  E I.M.G.
000400000000     H*--------------------------------------------------------------*
000500941017     FFNLR30D   CF   E             WORKSTN
000600941017     FFNFVV01L  IF   E           K DISK
000700161017     Ftabel00f  IF   E           K DISK
000800040109     Fazorg01l  IF   E           K DISK
000900941017     D*
001000161007     D TES             S             36    DIM(3) CTDATA PERRCD(1)              TESTATA
001100161007     D TIN             S             12    DIM(3) CTDATA PERRCD(1)              TIPO INV
001200161109     D MSG             S             78    DIM(11) CTDATA PERRCD(1)              MSG DI ERRORE
001300961016     D C78             S              1    DIM(78)                              DI COMODO
001400911015     D*
001500931228     D*** VEDO SE LANCIO IMG O IMA - DATI DA AZIONE
001600931228     D PARAM1          DS
001700931228     D  PARFLG               100    100
001800941017     D*** PASSO DATI A ELABORAZIONE BATCH        - FNLR31R -
001900900523     D PARAM           DS
002000911015     D  VIDNFV                 1      5
002100911015     D  VIDSN                  6      6
002200941028     D* LINEE IN GESTIONE DA CONTROLLARE CON LE SPUNTE IMA
002300920813     D  VIDFIL                 7      9  0
002400941028     D* FILIALE APERTURA FOGLIO
002500941028     D  VIDFLF                10     12  0
002600950810     D* TIPO INVENTARIO
002700950810     D  PARTIN                13     13
002800160502     D* libero
002900160502     D  PAR_lib               14     14
003000060508     D* linea di arrivo
003100060508     D  vidlna                15     17  0
003200070418     D* anche colli mancanti di spedizioni in distinta - solo IMA
003300070418     D  vidmdc                18     18
003400101215     D* solo colli spuntati alla lna finale            - solo IMA
003500101215     D  parimaa               19     19
003600160502     D* TIPO zone consegna
003700160502     D  PARTpm1               20     21
003800160502     D  PARTpm2               22     23
003900941028     D***
004000941017     D*** PARAMETRI PER STAMPA ANOMALIE          - FNLR35R -
004100921016     D PARAM4          DS
004200921016     D  PA4IE                  1      1
004300921016     D  PA4SN                  2      2
004400921016     D  PA4RIS                 3      3
004500921016     D  PA4STA                 4      4
004600921016     D* FASE APERTURA
004700921016     D  PA4FAS                 5      5
004800921016     D* DATE APERTURA
004900941018     D  PA4DAD                11     16  0
005000941018     D  PA4DAA                17     22  0
005100941018     D  CO4DAD                23     28  0
005200941018     D  CO4DAA                29     34  0
005300941018     D* INCLUSIONE ESCLUSIONE ANOMALIE
005400941018     D  PA4INC                35     35
005500941018     D  ANM                   36     65
005600921022     D                                     DIM(10)                              ANOMALIE
005700941018     D* RICHIESTA FOGLIO
005800941018     D  PA4NFV                66     70  0
005900941018     D  PA4DFV                71     76  0
006000950810     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
006100950810     D                 DS
006200950810     D  PA5TLA                 1      1
006300950810     D  PA5NPG                 2      2
006400950810     D  PA5SPG                 3      3
006500950810     D  PA5FAS                 4      4
006600950810     D  PARAM5                 1      4
006700941028     D*
006800941028     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
006900941028     D DSLV50        E DS                  EXTNAME(FNLV50DS)
007000161125     d
007100161125     d TRULVPODS     e ds
007200161125     D  skFilOk               16    765    dim(250)
007300161125     d
007400941028     D*
007500161017     D ds7b          E DS
007600161017     D CNCR80        E DS
007700941017     D DSLR01        E DS
007800900517     D KPJBA         E DS
007900921016     D WLBDAT          DS
008000941017     D  G02DAT                 1      8  0
008100941017     D  G02INV                 9     16  0
008200941017     D  G02ERR                17     17
008300941017     D  G02TGI                18     22  0
008400921016     D WGIDAT          DS
008500941017     D  GIODAT                 1      8  0
008600941017     D  GIOINV                 9     16  0
008700941017     D  GIOTGI                17     21  0
008800941018     D                 DS
008900941018     D  WDATA                  1      6  0
009000941018     D  AA                     1      2  0
009100941018     D  GG                     5      6  0
009200040108     D og143         E DS
009300050510     D og150         E DS
009400040108     D DS7N          E DS
009500060508     D FNLV55DS      E DS
009600920813     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009700040108     d wtfaas          S                   LIKE(§ogas)
009800161017     d Wuin            s              1
009900900702     C****************************************************************
010000900702     C*  RIEPILOGO INDICATORI
010100900702     C***************************************************************
010200161031     C* 01    - OFF  - I.M.A.    ON - I.M.G.    01 E 11 ON - I.P.G.
010300050510     C* 02    - Proposta per scelta zona parcel o messaggerie solo per IMA
010400050510     c*         e solo per P.O. abilitati
010500101215     C* 03    - Proposta per scelta solo colli spuntati alla lna finale
010600160502     C* 04    -
010700160502     C* 05    - 2° livello
010800160502     C* 06    - Proposta per scelta zona parcel 1° e/o 2° turno solo per IMA
010900160502     c*         e solo per P.O. abilitati
011000920813     C* 09    - INTERROGAZIONE FOGLI
011100161109     C* 11    - I P G
011200920813     C* 40/50 - ERRORE
011300900702     C* 31/32 - DI COMODO
011400900702     C*****************************************************************
011500000000     C     *ENTRY        PLIST
011600000000     C                   PARM                    KPJBA
011700920813     C                   MOVEL     KPJBU         PARAM1
011800941017     C*
011900000000     C                   Z-ADD     1             CODUT
012000920813     C                   CALL      'X§PARUT'
012100920813     C                   PARM                    UT§DSE
012200920813     C                   MOVEL     REC80         CNCR80
012300941017     C                   MOVEL     RAGUT         RSUT
012400900511     C*---------------------------------------------------------------*
012500920813     C*
012600920813     C                   EXSR      OPERAZ
012700161125
012800161125     c* per ora solo EDP e la filiale abilitate posso utilizzare la funzione
012900161125     c                   clear                   trulvpods
013000161125     c                   eval      ivpoke1 = 'DECOFIIPG'
013100161125     c                   call      'TRULVPOR'
013200161125     c                   parm                    trulvpods
013300161125     c
013400161017     c                   if        *in11 and wuin='1'
013500161125     c                   if        %subst(knmus:1:3)<>'EDP' and
013600161125     c                             %lookup('999':skFilOk)=0    and
013700161125     c                             %lookup(%editc(simpou:'X'):skFilOk)=0
013800161017     c                   seton                                        129028
013900161017     c                   movel     msg(10)       v1cmsg
014000161017     c                   endif
014100161017     c                   endif
014200911016     C*
014300900509     C     FOR01         TAG
014400941017     C                   EXFMT     LR30D01
014500941028     C*
014600941028     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
014700941028     C                   SETOFF                                       492851
014800101215     C                   SETOFF                                       444546
014900101215     C                   SETOFF                                       47
015000941017     C*
015100161007     C** F3 - FINE LAVORO
015200161017     c   12
015300161017     Cor KC              GOTO      FINE
015400941028     C**
015500161007     C** F18- CAMBIO FILIALE
015600920813     C   KS              SETON                                        04
015700920813     C   KS              GOTO      FOR01
015800920813     C*
015900060913     c                   if        *in04
016000060913     C                   EXSR      CTRFGS
016100060913     c                   if        not *in90
016200060913     c                   setoff                                       04
016300060913     c                   endif
016400060913     c
016500060913     C                   goto      for01
016600060913     c                   endif
016700920813     C*
016800910801     C*  CONTROLLI
016900910801     C                   EXSR      CONTR
017000941019     C  NKF
017100910801     COR 90              GOTO      FOR01
017200070418     C* F6 - ELABORAZIONE
017300950810     C                   CLEAR                   KPJBU
017400160502     c                   if        not *in01 and (*in02  or *in06)
017500160502     c                   eval      partpm1=vidtpm
017600160502     c                   eval      partpm2=vidtpm2
017700050510     c                   else
017800160502     c                   clear                   partpm1
017900160502     c                   clear                   partpm2
018000050510     c                   endif
018100160502
018200161007     c                   if        not *in01 and *in03
018300101215     c                   eval      parimaa=vidimaa
018400101215     c                   else
018500101215     c                   clear                   parimaa
018600101215     c                   endif
018700161007     c
018800900702     C                   MOVEL     PARAM         KPJBU
018900941017     C                   MOVEL     'LR31'        KCOAZ
019000900702     C                   CALL      'BCH10'
019100900702     C                   PARM                    KPJBA
019200921016     C**
019300921016     C* ANOMALIE
019400921016     C                   MOVEL     VIDNFV        PA4NFV                          FOGLIO
019500921016     C*
019600921016     C                   MOVE      FVVDFV        G02INV                          DATA DAL
019700921016     C                   MOVE      *ZEROS        G02DAT
019800921016     C                   MOVEL     3             G02ERR
019900941017     C                   CALL      'XSRDA8'
020000921016     C                   PARM                    WLBDAT
020100950317     C                   MOVEL     G02DAT        PA4DFV
020200950324     C                   MOVE      G02DAT        WAA               2 0
020300950324     C                   MOVE      WAA           PA4DFV
020400921016     C* SOTRAGGO 1 ALLA DATA FOGLIO
020500921016     C     G02TGI        SUB       1             GIOTGI
020600921016     C*
020700941017     C                   CALL      'XSRGI8'
020800921016     C                   PARM                    WGIDAT
020900921016     C*
021000941018     C                   Z-ADD     GIOINV        CO4DAD                          AA/MM/GG
021100941018     C                   Z-ADD     CO4DAD        WDATA                           DI COMODO
021200941018     C                   Z-ADD     CO4DAD        PA4DAD
021300941018     C                   MOVEL     GG            PA4DAD
021400941018     C                   MOVE      AA            PA4DAD                          GG/MM/AA
021500921016     C* SOMMO 1 AI GIORNI ALLA DATA FOGLIO
021600921016     C     G02TGI        ADD       1             GIOTGI                          DATA AL
021700941017     C                   CALL      'XSRGI8'
021800921016     C                   PARM                    WGIDAT
021900921016     C*
022000941018     C                   Z-ADD     GIOINV        CO4DAA                          AA/MM/GG
022100941018     C                   Z-ADD     CO4DAA        WDATA                           DI COMODO
022200941018     C                   Z-ADD     CO4DAA        PA4DAA
022300941018     C                   MOVEL     GG            PA4DAA
022400941018     C                   MOVE      AA            PA4DAA                          GG/MM/AA
022500921016     C*
022600950810     C                   CLEAR                   KPJBU
022700960529     C* IMPOSTO LA KPJBU
022800941028     C                   CLEAR                   DSLR01
022900941028     C                   Z-ADD     3             DLRNPG                          CATEGORIA
023000941028     C                   MOVEL     VIDNFV        DLRNFV                          NUM FOGLIO
023100941028     C                   Z-ADD     *ZERO         DLRNPD                          PISTOLA INIZIALE
023200941028     C                   Z-ADD     *HIVAL        DLRNPA                          PISTOLA FINALE
023300941028     C                   Z-ADD     *ZERO         DLRSRD                          SERIE INIZIALE
023400941028     C                   Z-ADD     *HIVAL        DLRSRA                          SERIE FINALE
023500941028     C                   MOVEL     VIDFLF        DLRFGS                          FIL APERT F
023600941028     C                   MOVEL     'S'           DLRFS1                          STAMPA SPU
023700950810     C                   CLEAR                   KPJBU
023800941028     C                   MOVEL     DSLR01        KPJBU
023900960529     C**
024000960529     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
024100960529     C     VIDSSP        IFEQ      'S'
024200941017     C                   MOVEL     'LR06'        KCOAZ
024300920930     C                   CALL      'BCH10'
024400920930     C                   PARM                    KPJBA
024500920930     C                   END
024600971128     C*
024700971128     C                   MOVEL     'C'           D55TLA
024800971128     C                   CALL      'FNLV55R'
024900060508     C                   PARM                    fnlv55ds
025000910320     C*
025100000000     C     FINE          TAG
025200000000     C                   SETON                                        LR
025300941017     C**************************************************************************
025400941017     C* OPERAZIONI INIZIALI
025500941017     C**************************************************************************
025600920813     C     OPERAZ        BEGSR
025700920813     C*
025800020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
025900920813     C*
026000020424     C                   Z-ADD     SIMPOU        VIDFIL
026100020424     C     SIMTPP        IFEQ      '2'
026200020424     C     SIMTPP        OREQ      *BLANKS
026300020424     C                   SETON                                        05
026400020424     C                   ENDIF
026500020424     C*
026600931228     C                   MOVEL     VIDFIL        VIDFLF
026700931228     C*
026800161017     C* 11 e 01 ON - C P G
026900161007     C                   SELECT
027000161007     C                   WHEN      PARFLG='Z'
027100161007     C                   SETON                                        1101
027200161017     c* Verifico se utilizzabile per tutti o solo per la 001 e edp
027300161017     C                   MOVEL     '7B'          COD
027400161017     C                   MOVEL(P)  parflg        key
027500161017     C     KTAB          chain     TABEL
027600161017     c                   if        not %found(tabel00f)
027700161017     c                   eval      Wuin='1'
027800161017     c                   else
027900161017     c                   movel     tbluni        ds7b
028000161017     c                   eval      Wuin=§7buti
028100161017     c                   endif
028200161017     c
028300161017     C* 01 ON - I M G
028400161007    1C                   WHEN      PARFLG='G'
028500931228     C                   SETON                                        01
028600161007   x1C                   OTHER
028700050510     c* Vedo se p.o. gestione abilitato al piking messaggerie
028800050510     c     vidfil        chain     azorg01l
028900050510    2c                   if        %found(azorg01l)
029000050510     c                   movel     orgdf0        og150
029100050510    3c                   if        §ogpkm='S'
029200050510     c                   seton                                        02
029300050510    3C                   END
029400101215    3c                   if        §ogimaa='S'
029500101215     c                   seton                                        03
029600101215    3C                   END
029700160502     c                   if        §ogturno='S'
029800160502     c                   seton                                        06
029900160502    3C                   END
030000101215     c
030100050510    2C                   END
030200161007    1C                   ENDSL
030300931228     C*
030400931228     C* CAMPI DI PASSAGGIO
030500931228     C*
030600931228     C  N01              MOVE      TES(1)        VIDTES
030700931228     C  N01              MOVEL     TIN(1)        VIDTIN
030800161007     C  N01              MOVEL     'A'           PA5SPG
030900161007     C                   IF        *IN01
031000161007     C  N11              MOVE      TES(2)        VIDTES
031100161007     C  N11              MOVEL     TIN(2)        VIDTIN
031200161007     C  N11              MOVEL     'G'           PA5SPG
031300161007     C   11              MOVE      TES(3)        VIDTES
031400161007     C   11              MOVEL     TIN(3)        VIDTIN
031500161007     C   11              MOVEL     'Z'           PA5SPG
031600161007     C                   ENDIF
031700931228     C                   Z-ADD     3             COMNPG
031800950810     C                   MOVEL     PA5SPG        PARTIN
031900931228     C*
032000931228     C* PRENDO FASE ANOMALIA
032100931228     C*
032200950810     C                   MOVEL     'L'           PA5TLA
032300950810     C                   MOVEL     3             PA5NPG
032400950810     C                   CLEAR                   PA5FAS
032500950810     C                   CALL      'TRUL11R'
032600950810     C                   PARM                    PARAM5
032700950810     C*
032800950810     C                   MOVEL     PA5FAS        PA4FAS            1
032900931228     C*
033000931228     C*
033100931228     C                   MOVEL     *BLANKS       VIDNFV
033200931228     C                   MOVEL     'N'           VIDSN
033300960430     C                   MOVEL     'N'           VIDSSP
033400060508     c                   clear                   vidlna
033500931228     C* PER ANOMALIE
033600931228     C                   MOVEA     *BLANKS       ANM
033700931228     C                   MOVEL     'I'           PA4IE
033800931228     C                   MOVEL     ' '           PA4SN
033900931228     C                   MOVEL     ' '           PA4RIS
034000931228     C                   MOVEL     'N'           PA4STA
034100931228     C                   MOVEL     'I'           PA4INC
034200070418     C                   MOVEL     'N'           vidmdc
034300110131     C  n03              MOVEL     'N'           vidimaa
034400110131     C   03              MOVEL     'S'           vidimaa
034500941017     C*
034600920813     C                   ENDSR
034700941028      *-----------------------------------------------------***********
034800941028      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
034900941028      *-----------------------------------------------------***********
035000941028     C     CTRFGS        BEGSR
035100941028     C                   SETOFF                                       90
035200941028     C                   CLEAR                   DSLV50
035300941028     C                   MOVEL     KNMUS         D50PRU
035400941028     C                   MOVEL     VIDFIL        D50FGS
035500941028     C                   CALL      'FNLV50R'
035600941028     C                   PARM                    DSLV50
035700941028     C*
035800941028     C     D50ERR        IFNE      ' '
035900941028     C                   MOVEL     D50MSG        V1CMSG
036000941028     C                   SETON                                        499028
036100941028     C                   GOTO      ENDCTF
036200941028     C                   ENDIF
036300050510     c* Verifico se ha piking messaggerie: abilito i disabilito
036400050510     c*  il campo
036500101215     c* Verifico se ha ima per colli spuntati in arrivo: abilito o disabilito
036600160906     c                   setoff                                       020306
036700101215     c
036800050510     c                   if        not *in01
036900050510     c     vidfil        chain     azorg01l
037000050510     c                   if        %found(azorg01l)
037100050510     c                   movel     orgdf0        og150
037200101215     c
037300101215    3c                   if        §ogpkm='S'
037400101215     c                   seton                                        02
037500101215    3C                   END
037600101215     c
037700101215     c                   if        §ogimaa='S'
037800101215     c                   seton                                        03
037900110131     c                   movel     'S'           vidimaa
038000050510     C                   END
038100160906     c                   if        §ogturno='S'
038200160906     c                   seton                                        06
038300160906    3C                   END
038400101215     c
038500050510     C                   END
038600101215     C                   END
038700941028     C*
038800941028     C     ENDCTF        ENDSR
038900941017     C**************************************************************************
039000941017     C* CONTROLLO CAMPI
039100941017     C**************************************************************************
039200910320     C     CONTR         BEGSR
039300941017     C*
039400920624     C                   SETOFF                                       9030
039500941028     C*
039600931228     C* FILIALE DI APETTURA FOGLIO
039700941028     C                   CLEAR                   DSLV50
039800941028     C                   MOVEL     KNMUS         D50PRU
039900941028     C                   MOVEL     VIDFLF        D50FGS
040000941028     C                   CALL      'FNLV50R'
040100941028     C                   PARM                    DSLV50
040200941028     C*
040300941028     C     D50ERR        IFNE      ' '
040400941028     C                   MOVEL     D50MSG        V1CMSG
040500941028     C                   SETON                                        519028
040600941028     C                   GOTO      ENDCT1
040700941028     C                   ENDIF
040800931228     C*
040900050510     C* NUMERO FOGLIO
041000911015     C     VIDNFV        IFEQ      *BLANKS
041100911015     C     VIDNFV        OREQ      *ZEROS
041200911015     C                   SETON                                        4090
041300911015     C                   GOTO      ENDCT1
041400911015     C*
041500911015     C                   ELSE
041600911015     C* RICERCA
041700931228     C     '?'           SCAN      VIDNFV                                 90
041800931228     C   90              DO
041900941017     C                   CLEAR                   DSLR01
042000941017     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
042100941028     C                   MOVEL     'FNLR30R'     DLRPGM                         PROGRAMMA
042200941017     C                   MOVEL     '2'           DLRTFV
042300941017     C                   MOVEL     VIDFLF        DLRFGS
042400941017     C                   MOVEL     3             DLRNPG
042500941017     C                   MOVE      31129999      DLRGAL
042600941017     C                   MOVE      99991231      DLRAAL
042700941017     C                   MOVEL     DSLR01        KPJBU
042800941017     C                   CALL      'FNLR02R'
042900911015     C                   PARM                    KPJBA
043000941017     C                   MOVEL     KPJBU         DSLR01
043100941017     C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
043200941017     C                   MOVEL     DLRNFV        VIDNFV
043300941017     C                   ENDIF
043400911016     C*
043500911015     C                   GOTO      ENDCT1
043600911015     C                   END
043700911016     C*  C O N T R O L L I
043800911015     C                   TESTN                   VIDNFV               31
043900930914     C     *IN31         IFEQ      *OFF
044000930914     C                   SETON                                        30
044100930914     C*
044200930914     C                   ELSE
044300911015     C*
044400930914     C                   MOVEL     VIDNFV        COMNFV
044500941017     C     KFVV          CHAIN     FNFVV01L                           30
044600911015     C* ANNULLATO
044700911015     C  N30FVVATB        IFNE      *BLANKS
044800911015     C                   SETON                                        30
044900911015     C                   END
045000930914     C                   ENDIF
045100911016     C* ERRORE
045200911015     C   30              SETON                                        4090
045300911015     C   30              GOTO      ENDCT1
045400920813     C*
045500911015     C* CHIUSO
045600930914     C     FVVFCF        IFNE      *BLANKS
045700911015     C                   SETON                                        4290
045800911016     C                   GOTO      ENDCT1
045900911015     C                   END
046000911016     C*
046100911016     C* NON E' INVENTARIO ARRIVI
046200950810     C  N01FVVSPG        IFNE      PA5SPG
046300911016     C                   SETON                                        4190
046400911016     C                   GOTO      ENDCT1
046500911016     C                   END
046600920813     C*
046700920813     C* NON E' INVENTARIO GIACENZE
046800161007     C                   IF        *IN01
046900161007     C  N11FVVSPG        IFNE      PA5SPG
047000920813     C                   SETON                                        4390
047100920813     C                   GOTO      ENDCT1
047200920813     C                   END
047300161031     C* PER IPG SI PUÒ UTILIZZARE SIA "G" CHE "Z"
047400161007     C   11              IF        FVVSPG<>'Z' AND FVVSPG<>'G'
047500161007     C                   SETON                                        902846
047600161007     C                   MOVEA     MSG(9)        v1cmsg
047700161007     C                   GOTO      ENDCT1
047800161007     C                   END
047900161007
048000161007     C                   ENDIF
048100911015     C*
048200911015     C                   END
048300050510     c
048400050510     c* Per elaborazione definitiva, accetto solo ' ' nella scelta
048500050510     c*  delle zone di consegna
048600160502     c                   if        not *in01 and  vidsn='S'
048700160502     c                             and (*in02 or *in06)
048800160502     c                   if        vidtpm<>'  ' or vidtpm2<>'  '
048900050510     C                   SETON                                        902844
049000050510     C                   MOVEA     MSG(2)        v1cmsg
049100060508     C                   GOTO      ENDCT1
049200050510     C                   END
049300160502     C                   END
049400160502     c*
049500160502     c                   if        not *in01 and not *in06  and *in02
049600160502     c                   if        vidtpm='P1' or vidtpm='P2' or
049700160502     c                             vidtpm2='P1' or vidtpm2='P2'
049800160502     C                   SETON                                        902844
049900160502     C                   MOVEA     MSG(7)        v1cmsg
050000160502     C                   GOTO      ENDCT1
050100160502     c                   endif
050200160502     c                   endif
050300160502     c                   if        not *in01 and *in06  and not *in02
050400160502     c                   if        vidtpm='P ' or vidtpm='M ' or
050500160502     c                             vidtpm2='P ' or vidtpm2='M '
050600160502     C                   SETON                                        902844
050700160502     C                   MOVEA     MSG(7)        v1cmsg
050800160502     C                   GOTO      ENDCT1
050900160502     c                   endif
051000160502     c                   endif
051100160504     c* non posso mai accettare P +P1 o P+P2
051200160504     c                   if        vidtpm='P ' or  vidtpm2='P '
051300160504     c                   if        vidtpm='P1' or vidtpm='P2' or
051400160504     c                             vidtpm2='P1' or vidtpm2='P2'
051500160504     C                   SETON                                        902844
051600160504     C                   MOVEA     MSG(8)        v1cmsg
051700160504     C                   GOTO      ENDCT1
051800160504     c                   endif
051900160504     c                   endif
052000160502
052100060508     c* non posso selezionare linea di arrivo se elaborazione definitiva
052200060508     c                   if        vidsn='S' and vidlna>0
052300060508     C                   SETON                                        902845
052400060508     C                   MOVEA     MSG(3)        v1cmsg
052500060508     C                   GOTO      ENDCT1
052600060508     C                   END
052700101215     c* non posso selezionare solo colli spuntati alla lna finale se elab.definitiva
052800101215     c                   if        vidsn='S' and vidimaa='S'
052900101215     C                   SETON                                        902847
053000101215     C                   MOVEA     MSG(6)        v1cmsg
053100101215     C                   GOTO      ENDCT1
053200101215     C                   END
053300060508     c* Deve essere gestita dal p.o. in gestione
053400060508     c                   if        vidlna>0 and vidlna<>vidfil
053500060508     c                   clear                   fnlv55ds
053600060508     c                   eval      d55tpt='6'
053700060508     c                   eval      d55lin=vidlna
053800060508     c                   eval      d55drf=fvvdfv
053900060508     c                   call      'FNLV55R'
054000060508     c                   parm                    fnlv55ds
054100060508     c
054200060508     c                   if        d55tfa<>vidfil
054300060508     C                   SETON                                        902845
054400060508     C                   MOVEl     MSG(4)        v1cmsg
054500060508     C                   GOTO      ENDCT1
054600060508     c                   endif
054700060508     c                   endif
054800070706     c* Per elaborazione definitiva non posso elanciare in data >
054900070706     c                   if        vidsn='S' and fvvdfv>dateu
055000070706     C                   SETON                                        902846
055100070706     C                   MOVEA     MSG(5)        v1cmsg
055200070706     C                   GOTO      ENDCT1
055300070706     C                   END
055400161109     c
055500161125     c* non posso fare elaborazione definitiva per IPG con foglio IMG
055600161109     c                   if        vidsn='S' and *in11 and fvvspg='G'
055700161109     C                   SETON                                        902846
055800161109     C                   MOVEA     MSG(11)       v1cmsg
055900161109     C                   GOTO      ENDCT1
056000161109     C                   ENDif
056100910320     C*
056200911015     C     ENDCT1        ENDSR
056300941017     C**************************************************************************
056400941017     C*    R O U T I N E     I N I Z I A L E
056500941017     C**************************************************************************
056600941017     C     *INZSR        BEGSR
056700941017     C*
056800941017     C     KFVV          KLIST
056900941017     C                   KFLD                    COMNPG
057000941017     C                   KFLD                    COMNFV
057100941028     C                   KFLD                    VIDFLF
057200161017     C     KTAB          KLIST
057300161017     C                   KFLD                    CODUT
057400161017     C                   KFLD                    COD               2
057500161017     C                   KFLD                    KEY               8
057600941017     C*
057700941017     C     *LIKE         DEFINE    FVVNPG        COMNPG                         CATEGORIA FOGLIO
057800941017     C     *LIKE         DEFINE    FVVNFV        COMNFV                         NR.FOGLIO
057900961017     C     *LIKE         DEFINE    VIDNFV        WNFV
058000971128     C     *LIKE         DEFINE    D55TFA        WFE0
058100971128     C     *LIKE         DEFINE    D55TFP        WTFP
058200070706     c* data del giorno
058300070706     C                   TIME                    W0140            14 0
058400070706     C* UDATE IN GGMMAAAA
058500070706     C                   MOVE      W0140         WDTGIO            8 0
058600070706     C*
058700070706     C* UDATE IN AAAAMMGG
058800070706     C                   Z-ADD     WDTGIO        G02DAT
058900070706     C                   MOVEL     *BLANK        G02ERR
059000070706     C                   CALL      'XSRDA8'
059100070706     C                   PARM                    WLBDAT
059200070706     C                   MOVEL     G02INV        DATEU             8 0
059300941017     C*
059400941017     C                   ENDSR
059500941017     C**************************************************************************
059600961016**
059700911018 ** INVENTARIO  MAGAZZINO  ARRIVI **
059800920813 ** INVENTARIO MAGAZZINO GIACENZE **
059900161007 * QUADRATURA COLLI A MAG.GIACENZE *
060000911018**
060100911018 I . M . A .
060200920813 I . M . G .
060300161031 I . P . G .
060400961016**  MSG
060500101215Manca l'elaboraz. dell'IDA del proprio terminal di arr. xxx. ENTER PER FORZARE     1
060600160504Per elaborazione DEFINITIVA non si può scegliere il tipo zona consegna             2
060700101215Per elaborazione DEFINITIVA non si può selezionare la linea di arrivo              3
060800101215Linea di Arrivo errata: non in gestione al P.O. selezionato                        4
060900101215Impossibile elaborazione definitiva per foglio in data MAGGIORE di oggi            5
061000101215Per elaborazione DEFINITIVA non selezionare solo colli spuntati alla LNA finale    6
061100160502Tipo scelta zone di consegna non prevista                                          7
061200160504Scelta tipi  zone di consegna non corretta                                         8
061300161017Il foglio per la quadratura colli a Mag.Giac. deve essere un tipo "Z" o "G"        9
061400161125Funzione abilitata soltanto in TEST per alcune filiali                            10
061500161110Impossibile chiudere foglio IMG dalla quadratura colli a mag.giacenze (IPG)       11
