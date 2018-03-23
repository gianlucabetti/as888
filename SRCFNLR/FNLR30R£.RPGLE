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
009400170331     D og148         E DS
009500040108     D DS7N          E DS
009600170331     D FNLV55DS      E DS
009700170331     D fnlg87ds      E DS
009800920813     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009900040108     d wtfaas          S                   LIKE(§ogas)
010000161017     d Wuin            s              1
010100900702     C****************************************************************
010200900702     C*  RIEPILOGO INDICATORI
010300900702     C***************************************************************
010400161031     C* 01    - OFF  - I.M.A.    ON - I.M.G.    01 E 11 ON - I.P.G.
010500050510     C* 02    - Proposta per scelta zona parcel o messaggerie solo per IMA
010600050510     c*         e solo per P.O. abilitati
010700101215     C* 03    - Proposta per scelta solo colli spuntati alla lna finale
010800160502     C* 04    -
010900160502     C* 05    - 2° livello
011000160502     C* 06    - Proposta per scelta zona parcel 1° e/o 2° turno solo per IMA
011100160502     c*         e solo per P.O. abilitati
011200920813     C* 09    - INTERROGAZIONE FOGLI
011300161109     C* 11    - I P G
011400920813     C* 40/50 - ERRORE
011500900702     C* 31/32 - DI COMODO
011600900702     C*****************************************************************
011700000000     C     *ENTRY        PLIST
011800000000     C                   PARM                    KPJBA
011900920813     C                   MOVEL     KPJBU         PARAM1
012000941017     C*
012100000000     C                   Z-ADD     1             CODUT
012200920813     C                   CALL      'X§PARUT'
012300920813     C                   PARM                    UT§DSE
012400920813     C                   MOVEL     REC80         CNCR80
012500941017     C                   MOVEL     RAGUT         RSUT
012600900511     C*---------------------------------------------------------------*
012700920813     C*
012800920813     C                   EXSR      OPERAZ
012900161125
013000161125     c* per ora solo EDP e la filiale abilitate posso utilizzare la funzione
013100161125     c                   clear                   trulvpods
013200161125     c                   eval      ivpoke1 = 'DECOFIIPG'
013300161125     c                   call      'TRULVPOR'
013400161125     c                   parm                    trulvpods
013500161125     c
013600170331     c***                if        *in11 and wuin='1'
013700170331     c***                if        %subst(knmus:1:3)<>'EDP' and
013800170331     c***                          %lookup('999':skFilOk)=0    and
013900170331     c***                          %lookup(%editc(simpou:'X'):skFilOk)=0
014000170331     c***                seton                                        129028
014100170331     c***                movel     msg(10)       v1cmsg
014200170331     c***                endif
014300170331     c***                endif
014400911016     C*
014500900509     C     FOR01         TAG
014600941017     C                   EXFMT     LR30D01
014700941028     C*
014800941028     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
014900941028     C                   SETOFF                                       492851
015000101215     C                   SETOFF                                       444546
015100101215     C                   SETOFF                                       47
015200941017     C*
015300161007     C** F3 - FINE LAVORO
015400170331     c***   12
015500170331     C***or KC              GOTO      FINE
015600170331     C   KC              GOTO      FINE
015700941028     C**
015800161007     C** F18- CAMBIO FILIALE
015900920813     C   KS              SETON                                        04
016000920813     C   KS              GOTO      FOR01
016100920813     C*
016200060913     c                   if        *in04
016300060913     C                   EXSR      CTRFGS
016400060913     c                   if        not *in90
016500060913     c                   setoff                                       04
016600060913     c                   endif
016700060913     c
016800060913     C                   goto      for01
016900060913     c                   endif
017000920813     C*
017100910801     C*  CONTROLLI
017200910801     C                   EXSR      CONTR
017300941019     C  NKF
017400910801     COR 90              GOTO      FOR01
017500070418     C* F6 - ELABORAZIONE
017600950810     C                   CLEAR                   KPJBU
017700160502     c                   if        not *in01 and (*in02  or *in06)
017800160502     c                   eval      partpm1=vidtpm
017900160502     c                   eval      partpm2=vidtpm2
018000050510     c                   else
018100160502     c                   clear                   partpm1
018200160502     c                   clear                   partpm2
018300050510     c                   endif
018400160502
018500161007     c                   if        not *in01 and *in03
018600101215     c                   eval      parimaa=vidimaa
018700101215     c                   else
018800101215     c                   clear                   parimaa
018900101215     c                   endif
019000170331
019100170331     c* Per elaborazione definitiva dell'IMA se partito con procedura, d in FD
019200170331     c*  sottometto la stampa delle sole sped in FD per chi ha il picking
019300170331     c*  e delle zone messaggerie per chi ha il misto picking.
019400170331     c*  Per chi fa l'idd il giorno dopo lo faccio solo nel provvisorio
019500170331    0c  n01              if        %lookup('999':skFilOk)=0    and
019600170331     c                             %lookup(%editc(vidfil:'X'):skFilOk)=0
019700170331   x0c                   else
019800170331    1c                   if        (§ogeid<>'S' and vidsn='S') or
019900170331     c                             (§ogeid= 'S' and vidsn='N')
020000170331     c                   clear                   fnlg87ds
020100170331     c                   eval      D87TST='1'
020200170331     c                   movel     vidfil        d87fgs
020300170331
020400170331    2c                   select
020500170331     c                   when      §ogpck='S' and §ogpkm='S'
020600170331     c                   eval      d87fdep='S'
020700170331     c                   eval      d87mes ='S'
020800170331     c                   when      §ogpck='S' and §ogpkm=' '
020900170331     c                   eval      d87fdep='S'
021000170331     c                   when      §ogpck=' ' and §ogpkm='S'
021100170331     c                   eval      d87mes ='S'
021200170331    2c                   endsl
021300170331     c
021400170331    2c                   if        d87fdep<>' ' or d87mes<>' '
021500170331     c                   movel(p)  fnlg87ds      kpjbu
021600170331     C                   MOVEL     'LG86'        KCOAZ
021700170331     C                   CALL      'BCH10'
021800170331     C                   PARM                    KPJBA
021900170331    2c                   endif
022000170331    1c                   endif
022100170331    0c                   endif
022200170331     c
022300161007     c
022400170331     C                   MOVEL(P)  PARAM         KPJBU
022500941017     C                   MOVEL     'LR31'        KCOAZ
022600900702     C                   CALL      'BCH10'
022700900702     C                   PARM                    KPJBA
022800921016     C**
022900921016     C* ANOMALIE
023000921016     C                   MOVEL     VIDNFV        PA4NFV                          FOGLIO
023100921016     C*
023200921016     C                   MOVE      FVVDFV        G02INV                          DATA DAL
023300921016     C                   MOVE      *ZEROS        G02DAT
023400921016     C                   MOVEL     3             G02ERR
023500941017     C                   CALL      'XSRDA8'
023600921016     C                   PARM                    WLBDAT
023700950317     C                   MOVEL     G02DAT        PA4DFV
023800950324     C                   MOVE      G02DAT        WAA               2 0
023900950324     C                   MOVE      WAA           PA4DFV
024000921016     C* SOTRAGGO 1 ALLA DATA FOGLIO
024100921016     C     G02TGI        SUB       1             GIOTGI
024200921016     C*
024300941017     C                   CALL      'XSRGI8'
024400921016     C                   PARM                    WGIDAT
024500921016     C*
024600941018     C                   Z-ADD     GIOINV        CO4DAD                          AA/MM/GG
024700941018     C                   Z-ADD     CO4DAD        WDATA                           DI COMODO
024800941018     C                   Z-ADD     CO4DAD        PA4DAD
024900941018     C                   MOVEL     GG            PA4DAD
025000941018     C                   MOVE      AA            PA4DAD                          GG/MM/AA
025100921016     C* SOMMO 1 AI GIORNI ALLA DATA FOGLIO
025200921016     C     G02TGI        ADD       1             GIOTGI                          DATA AL
025300941017     C                   CALL      'XSRGI8'
025400921016     C                   PARM                    WGIDAT
025500921016     C*
025600941018     C                   Z-ADD     GIOINV        CO4DAA                          AA/MM/GG
025700941018     C                   Z-ADD     CO4DAA        WDATA                           DI COMODO
025800941018     C                   Z-ADD     CO4DAA        PA4DAA
025900941018     C                   MOVEL     GG            PA4DAA
026000941018     C                   MOVE      AA            PA4DAA                          GG/MM/AA
026100921016     C*
026200950810     C                   CLEAR                   KPJBU
026300960529     C* IMPOSTO LA KPJBU
026400941028     C                   CLEAR                   DSLR01
026500941028     C                   Z-ADD     3             DLRNPG                          CATEGORIA
026600941028     C                   MOVEL     VIDNFV        DLRNFV                          NUM FOGLIO
026700941028     C                   Z-ADD     *ZERO         DLRNPD                          PISTOLA INIZIALE
026800941028     C                   Z-ADD     *HIVAL        DLRNPA                          PISTOLA FINALE
026900941028     C                   Z-ADD     *ZERO         DLRSRD                          SERIE INIZIALE
027000941028     C                   Z-ADD     *HIVAL        DLRSRA                          SERIE FINALE
027100941028     C                   MOVEL     VIDFLF        DLRFGS                          FIL APERT F
027200941028     C                   MOVEL     'S'           DLRFS1                          STAMPA SPU
027300950810     C                   CLEAR                   KPJBU
027400941028     C                   MOVEL     DSLR01        KPJBU
027500960529     C**
027600960529     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
027700960529     C     VIDSSP        IFEQ      'S'
027800941017     C                   MOVEL     'LR06'        KCOAZ
027900920930     C                   CALL      'BCH10'
028000920930     C                   PARM                    KPJBA
028100920930     C                   END
028200971128     C*
028300971128     C                   MOVEL     'C'           D55TLA
028400971128     C                   CALL      'FNLV55R'
028500060508     C                   PARM                    fnlv55ds
028600910320     C*
028700000000     C     FINE          TAG
028800000000     C                   SETON                                        LR
028900941017     C**************************************************************************
029000941017     C* OPERAZIONI INIZIALI
029100941017     C**************************************************************************
029200920813     C     OPERAZ        BEGSR
029300920813     C*
029400020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
029500920813     C*
029600020424     C                   Z-ADD     SIMPOU        VIDFIL
029700020424     C     SIMTPP        IFEQ      '2'
029800020424     C     SIMTPP        OREQ      *BLANKS
029900020424     C                   SETON                                        05
030000020424     C                   ENDIF
030100020424     C*
030200931228     C                   MOVEL     VIDFIL        VIDFLF
030300931228     C*
030400161017     C* 11 e 01 ON - C P G
030500161007     C                   SELECT
030600161007     C                   WHEN      PARFLG='Z'
030700161007     C                   SETON                                        1101
030800161017     c* Verifico se utilizzabile per tutti o solo per la 001 e edp
030900161017     C                   MOVEL     '7B'          COD
031000161017     C                   MOVEL(P)  parflg        key
031100161017     C     KTAB          chain     TABEL
031200161017     c                   if        not %found(tabel00f)
031300161017     c                   eval      Wuin='1'
031400161017     c                   else
031500161017     c                   movel     tbluni        ds7b
031600161017     c                   eval      Wuin=§7buti
031700161017     c                   endif
031800161017     c
031900161017     C* 01 ON - I M G
032000161007    1C                   WHEN      PARFLG='G'
032100931228     C                   SETON                                        01
032200161007   x1C                   OTHER
032300050510     c* Vedo se p.o. gestione abilitato al piking messaggerie
032400050510     c     vidfil        chain     azorg01l
032500050510    2c                   if        %found(azorg01l)
032600050510     c                   movel     orgdf0        og150
032700170331     c* Verifico se fa l'IDD il giorno dopo
032800170331     c                   movel     orgde8        og148
032900050510    3c                   if        §ogpkm='S'
033000050510     c                   seton                                        02
033100050510    3C                   END
033200101215    3c                   if        §ogimaa='S'
033300101215     c                   seton                                        03
033400101215    3C                   END
033500160502     c                   if        §ogturno='S'
033600160502     c                   seton                                        06
033700160502    3C                   END
033800101215     c
033900050510    2C                   END
034000161007    1C                   ENDSL
034100931228     C*
034200931228     C* CAMPI DI PASSAGGIO
034300931228     C*
034400931228     C  N01              MOVE      TES(1)        VIDTES
034500931228     C  N01              MOVEL     TIN(1)        VIDTIN
034600161007     C  N01              MOVEL     'A'           PA5SPG
034700161007     C                   IF        *IN01
034800161007     C  N11              MOVE      TES(2)        VIDTES
034900161007     C  N11              MOVEL     TIN(2)        VIDTIN
035000161007     C  N11              MOVEL     'G'           PA5SPG
035100161007     C   11              MOVE      TES(3)        VIDTES
035200161007     C   11              MOVEL     TIN(3)        VIDTIN
035300161007     C   11              MOVEL     'Z'           PA5SPG
035400161007     C                   ENDIF
035500931228     C                   Z-ADD     3             COMNPG
035600950810     C                   MOVEL     PA5SPG        PARTIN
035700931228     C*
035800931228     C* PRENDO FASE ANOMALIA
035900931228     C*
036000950810     C                   MOVEL     'L'           PA5TLA
036100950810     C                   MOVEL     3             PA5NPG
036200950810     C                   CLEAR                   PA5FAS
036300950810     C                   CALL      'TRUL11R'
036400950810     C                   PARM                    PARAM5
036500950810     C*
036600950810     C                   MOVEL     PA5FAS        PA4FAS            1
036700931228     C*
036800931228     C*
036900931228     C                   MOVEL     *BLANKS       VIDNFV
037000931228     C                   MOVEL     'N'           VIDSN
037100960430     C                   MOVEL     'N'           VIDSSP
037200060508     c                   clear                   vidlna
037300931228     C* PER ANOMALIE
037400931228     C                   MOVEA     *BLANKS       ANM
037500931228     C                   MOVEL     'I'           PA4IE
037600931228     C                   MOVEL     ' '           PA4SN
037700931228     C                   MOVEL     ' '           PA4RIS
037800931228     C                   MOVEL     'N'           PA4STA
037900931228     C                   MOVEL     'I'           PA4INC
038000070418     C                   MOVEL     'N'           vidmdc
038100110131     C  n03              MOVEL     'N'           vidimaa
038200110131     C   03              MOVEL     'S'           vidimaa
038300941017     C*
038400920813     C                   ENDSR
038500941028      *-----------------------------------------------------***********
038600941028      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
038700941028      *-----------------------------------------------------***********
038800941028     C     CTRFGS        BEGSR
038900941028     C                   SETOFF                                       90
039000941028     C                   CLEAR                   DSLV50
039100941028     C                   MOVEL     KNMUS         D50PRU
039200941028     C                   MOVEL     VIDFIL        D50FGS
039300941028     C                   CALL      'FNLV50R'
039400941028     C                   PARM                    DSLV50
039500941028     C*
039600941028     C     D50ERR        IFNE      ' '
039700941028     C                   MOVEL     D50MSG        V1CMSG
039800941028     C                   SETON                                        499028
039900941028     C                   GOTO      ENDCTF
040000941028     C                   ENDIF
040100050510     c* Verifico se ha piking messaggerie: abilito i disabilito
040200050510     c*  il campo
040300101215     c* Verifico se ha ima per colli spuntati in arrivo: abilito o disabilito
040400160906     c                   setoff                                       020306
040500101215     c
040600170331    1c                   if        not *in01
040700050510     c     vidfil        chain     azorg01l
040800170331    2c                   if        %found(azorg01l)
040900050510     c                   movel     orgdf0        og150
041000170331     c* Verifico se fa l'IDD il giorno dopo
041100170331     c                   movel     orgde8        og148
041200101215     c
041300101215    3c                   if        §ogpkm='S'
041400101215     c                   seton                                        02
041500101215    3C                   END
041600101215     c
041700101215     c                   if        §ogimaa='S'
041800101215     c                   seton                                        03
041900110131     c                   movel     'S'           vidimaa
042000050510     C                   END
042100160906     c                   if        §ogturno='S'
042200160906     c                   seton                                        06
042300160906    3C                   END
042400170331     c
042500170331    2C                   END
042600170331    1C                   END
042700941028     C*
042800941028     C     ENDCTF        ENDSR
042900941017     C**************************************************************************
043000941017     C* CONTROLLO CAMPI
043100941017     C**************************************************************************
043200910320     C     CONTR         BEGSR
043300941017     C*
043400920624     C                   SETOFF                                       9030
043500941028     C*
043600931228     C* FILIALE DI APETTURA FOGLIO
043700941028     C                   CLEAR                   DSLV50
043800941028     C                   MOVEL     KNMUS         D50PRU
043900941028     C                   MOVEL     VIDFLF        D50FGS
044000941028     C                   CALL      'FNLV50R'
044100941028     C                   PARM                    DSLV50
044200941028     C*
044300941028     C     D50ERR        IFNE      ' '
044400941028     C                   MOVEL     D50MSG        V1CMSG
044500941028     C                   SETON                                        519028
044600941028     C                   GOTO      ENDCT1
044700941028     C                   ENDIF
044800931228     C*
044900050510     C* NUMERO FOGLIO
045000911015     C     VIDNFV        IFEQ      *BLANKS
045100911015     C     VIDNFV        OREQ      *ZEROS
045200911015     C                   SETON                                        4090
045300911015     C                   GOTO      ENDCT1
045400911015     C*
045500911015     C                   ELSE
045600911015     C* RICERCA
045700931228     C     '?'           SCAN      VIDNFV                                 90
045800931228     C   90              DO
045900941017     C                   CLEAR                   DSLR01
046000941017     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
046100941028     C                   MOVEL     'FNLR30R'     DLRPGM                         PROGRAMMA
046200941017     C                   MOVEL     '2'           DLRTFV
046300941017     C                   MOVEL     VIDFLF        DLRFGS
046400941017     C                   MOVEL     3             DLRNPG
046500941017     C                   MOVE      31129999      DLRGAL
046600941017     C                   MOVE      99991231      DLRAAL
046700941017     C                   MOVEL     DSLR01        KPJBU
046800941017     C                   CALL      'FNLR02R'
046900911015     C                   PARM                    KPJBA
047000941017     C                   MOVEL     KPJBU         DSLR01
047100941017     C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
047200941017     C                   MOVEL     DLRNFV        VIDNFV
047300941017     C                   ENDIF
047400911016     C*
047500911015     C                   GOTO      ENDCT1
047600911015     C                   END
047700911016     C*  C O N T R O L L I
047800911015     C                   TESTN                   VIDNFV               31
047900930914     C     *IN31         IFEQ      *OFF
048000930914     C                   SETON                                        30
048100930914     C*
048200930914     C                   ELSE
048300911015     C*
048400930914     C                   MOVEL     VIDNFV        COMNFV
048500941017     C     KFVV          CHAIN     FNFVV01L                           30
048600911015     C* ANNULLATO
048700911015     C  N30FVVATB        IFNE      *BLANKS
048800911015     C                   SETON                                        30
048900911015     C                   END
049000930914     C                   ENDIF
049100911016     C* ERRORE
049200911015     C   30              SETON                                        4090
049300911015     C   30              GOTO      ENDCT1
049400920813     C*
049500911015     C* CHIUSO
049600930914     C     FVVFCF        IFNE      *BLANKS
049700911015     C                   SETON                                        4290
049800911016     C                   GOTO      ENDCT1
049900911015     C                   END
050000911016     C*
050100911016     C* NON E' INVENTARIO ARRIVI
050200950810     C  N01FVVSPG        IFNE      PA5SPG
050300911016     C                   SETON                                        4190
050400911016     C                   GOTO      ENDCT1
050500911016     C                   END
050600920813     C*
050700920813     C* NON E' INVENTARIO GIACENZE
050800161007     C                   IF        *IN01
050900161007     C  N11FVVSPG        IFNE      PA5SPG
051000920813     C                   SETON                                        4390
051100920813     C                   GOTO      ENDCT1
051200920813     C                   END
051300161031     C* PER IPG SI PUÒ UTILIZZARE SIA "G" CHE "Z"
051400161007     C   11              IF        FVVSPG<>'Z' AND FVVSPG<>'G'
051500161007     C                   SETON                                        902846
051600161007     C                   MOVEA     MSG(9)        v1cmsg
051700161007     C                   GOTO      ENDCT1
051800161007     C                   END
051900161007
052000161007     C                   ENDIF
052100911015     C*
052200911015     C                   END
052300050510     c
052400050510     c* Per elaborazione definitiva, accetto solo ' ' nella scelta
052500050510     c*  delle zone di consegna
052600160502     c                   if        not *in01 and  vidsn='S'
052700160502     c                             and (*in02 or *in06)
052800160502     c                   if        vidtpm<>'  ' or vidtpm2<>'  '
052900050510     C                   SETON                                        902844
053000050510     C                   MOVEA     MSG(2)        v1cmsg
053100060508     C                   GOTO      ENDCT1
053200050510     C                   END
053300160502     C                   END
053400160502     c*
053500160502     c                   if        not *in01 and not *in06  and *in02
053600160502     c                   if        vidtpm='P1' or vidtpm='P2' or
053700160502     c                             vidtpm2='P1' or vidtpm2='P2'
053800160502     C                   SETON                                        902844
053900160502     C                   MOVEA     MSG(7)        v1cmsg
054000160502     C                   GOTO      ENDCT1
054100160502     c                   endif
054200160502     c                   endif
054300160502     c                   if        not *in01 and *in06  and not *in02
054400160502     c                   if        vidtpm='P ' or vidtpm='M ' or
054500160502     c                             vidtpm2='P ' or vidtpm2='M '
054600160502     C                   SETON                                        902844
054700160502     C                   MOVEA     MSG(7)        v1cmsg
054800160502     C                   GOTO      ENDCT1
054900160502     c                   endif
055000160502     c                   endif
055100160504     c* non posso mai accettare P +P1 o P+P2
055200160504     c                   if        vidtpm='P ' or  vidtpm2='P '
055300160504     c                   if        vidtpm='P1' or vidtpm='P2' or
055400160504     c                             vidtpm2='P1' or vidtpm2='P2'
055500160504     C                   SETON                                        902844
055600160504     C                   MOVEA     MSG(8)        v1cmsg
055700160504     C                   GOTO      ENDCT1
055800160504     c                   endif
055900160504     c                   endif
056000160502
056100060508     c* non posso selezionare linea di arrivo se elaborazione definitiva
056200060508     c                   if        vidsn='S' and vidlna>0
056300060508     C                   SETON                                        902845
056400060508     C                   MOVEA     MSG(3)        v1cmsg
056500060508     C                   GOTO      ENDCT1
056600060508     C                   END
056700101215     c* non posso selezionare solo colli spuntati alla lna finale se elab.definitiva
056800101215     c                   if        vidsn='S' and vidimaa='S'
056900101215     C                   SETON                                        902847
057000101215     C                   MOVEA     MSG(6)        v1cmsg
057100101215     C                   GOTO      ENDCT1
057200101215     C                   END
057300060508     c* Deve essere gestita dal p.o. in gestione
057400060508     c                   if        vidlna>0 and vidlna<>vidfil
057500060508     c                   clear                   fnlv55ds
057600060508     c                   eval      d55tpt='6'
057700060508     c                   eval      d55lin=vidlna
057800060508     c                   eval      d55drf=fvvdfv
057900060508     c                   call      'FNLV55R'
058000060508     c                   parm                    fnlv55ds
058100060508     c
058200060508     c                   if        d55tfa<>vidfil
058300060508     C                   SETON                                        902845
058400060508     C                   MOVEl     MSG(4)        v1cmsg
058500060508     C                   GOTO      ENDCT1
058600060508     c                   endif
058700060508     c                   endif
058800070706     c* Per elaborazione definitiva non posso elanciare in data >
058900070706     c                   if        vidsn='S' and fvvdfv>dateu
059000070706     C                   SETON                                        902846
059100070706     C                   MOVEA     MSG(5)        v1cmsg
059200070706     C                   GOTO      ENDCT1
059300070706     C                   END
059400161109     c
059500161125     c* non posso fare elaborazione definitiva per IPG con foglio IMG
059600161109     c                   if        vidsn='S' and *in11 and fvvspg='G'
059700161109     C                   SETON                                        902846
059800161109     C                   MOVEA     MSG(11)       v1cmsg
059900161109     C                   GOTO      ENDCT1
060000161109     C                   ENDif
060100910320     C*
060200911015     C     ENDCT1        ENDSR
060300941017     C**************************************************************************
060400941017     C*    R O U T I N E     I N I Z I A L E
060500941017     C**************************************************************************
060600941017     C     *INZSR        BEGSR
060700941017     C*
060800941017     C     KFVV          KLIST
060900941017     C                   KFLD                    COMNPG
061000941017     C                   KFLD                    COMNFV
061100941028     C                   KFLD                    VIDFLF
061200161017     C     KTAB          KLIST
061300161017     C                   KFLD                    CODUT
061400161017     C                   KFLD                    COD               2
061500161017     C                   KFLD                    KEY               8
061600941017     C*
061700941017     C     *LIKE         DEFINE    FVVNPG        COMNPG                         CATEGORIA FOGLIO
061800941017     C     *LIKE         DEFINE    FVVNFV        COMNFV                         NR.FOGLIO
061900961017     C     *LIKE         DEFINE    VIDNFV        WNFV
062000971128     C     *LIKE         DEFINE    D55TFA        WFE0
062100971128     C     *LIKE         DEFINE    D55TFP        WTFP
062200070706     c* data del giorno
062300070706     C                   TIME                    W0140            14 0
062400070706     C* UDATE IN GGMMAAAA
062500070706     C                   MOVE      W0140         WDTGIO            8 0
062600070706     C*
062700070706     C* UDATE IN AAAAMMGG
062800070706     C                   Z-ADD     WDTGIO        G02DAT
062900070706     C                   MOVEL     *BLANK        G02ERR
063000070706     C                   CALL      'XSRDA8'
063100070706     C                   PARM                    WLBDAT
063200070706     C                   MOVEL     G02INV        DATEU             8 0
063300941017     C*
063400941017     C                   ENDSR
063500941017     C**************************************************************************
063600961016**
063700911018 ** INVENTARIO  MAGAZZINO  ARRIVI **
063800920813 ** INVENTARIO MAGAZZINO GIACENZE **
063900161007 * QUADRATURA COLLI A MAG.GIACENZE *
064000911018**
064100911018 I . M . A .
064200920813 I . M . G .
064300161031 I . P . G .
064400961016**  MSG
064500101215Manca l'elaboraz. dell'IDA del proprio terminal di arr. xxx. ENTER PER FORZARE     1
064600160504Per elaborazione DEFINITIVA non si può scegliere il tipo zona consegna             2
064700101215Per elaborazione DEFINITIVA non si può selezionare la linea di arrivo              3
064800101215Linea di Arrivo errata: non in gestione al P.O. selezionato                        4
064900101215Impossibile elaborazione definitiva per foglio in data MAGGIORE di oggi            5
065000101215Per elaborazione DEFINITIVA non selezionare solo colli spuntati alla LNA finale    6
065100160502Tipo scelta zone di consegna non prevista                                          7
065200160504Scelta tipi  zone di consegna non corretta                                         8
065300161017Il foglio per la quadratura colli a Mag.Giac. deve essere un tipo "Z" o "G"        9
065400161125Funzione abilitata soltanto in TEST per alcune filiali                            10
065500161110Impossibile chiudere foglio IMG dalla quadratura colli a mag.giacenze (IPG)       11
