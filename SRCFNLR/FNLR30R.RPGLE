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
021500180119     c                   eval      d87ord='L'
021600180119     c                   if        §ogpck=' '
021700180119     c                   eval      d87ord='Z'
021800180119     c                   endif
021900180119     c                   if        §ogntw='MES'
022000180119     c                   eval      d87ord='Z'
022100180119     c                   endif
022200180119     c
022300180119     c* Imposto ordinamento di stampa :
022400170331     c                   movel(p)  fnlg87ds      kpjbu
022500170331     C                   MOVEL     'LG86'        KCOAZ
022600170331     C                   CALL      'BCH10'
022700170331     C                   PARM                    KPJBA
022800170331    2c                   endif
022900170331    1c                   endif
023000170331    0c                   endif
023100170331     c
023200161007     c
023300170331     C                   MOVEL(P)  PARAM         KPJBU
023400941017     C                   MOVEL     'LR31'        KCOAZ
023500900702     C                   CALL      'BCH10'
023600900702     C                   PARM                    KPJBA
023700921016     C**
023800921016     C* ANOMALIE
023900921016     C                   MOVEL     VIDNFV        PA4NFV                          FOGLIO
024000921016     C*
024100921016     C                   MOVE      FVVDFV        G02INV                          DATA DAL
024200921016     C                   MOVE      *ZEROS        G02DAT
024300921016     C                   MOVEL     3             G02ERR
024400941017     C                   CALL      'XSRDA8'
024500921016     C                   PARM                    WLBDAT
024600950317     C                   MOVEL     G02DAT        PA4DFV
024700950324     C                   MOVE      G02DAT        WAA               2 0
024800950324     C                   MOVE      WAA           PA4DFV
024900921016     C* SOTRAGGO 1 ALLA DATA FOGLIO
025000921016     C     G02TGI        SUB       1             GIOTGI
025100921016     C*
025200941017     C                   CALL      'XSRGI8'
025300921016     C                   PARM                    WGIDAT
025400921016     C*
025500941018     C                   Z-ADD     GIOINV        CO4DAD                          AA/MM/GG
025600941018     C                   Z-ADD     CO4DAD        WDATA                           DI COMODO
025700941018     C                   Z-ADD     CO4DAD        PA4DAD
025800941018     C                   MOVEL     GG            PA4DAD
025900941018     C                   MOVE      AA            PA4DAD                          GG/MM/AA
026000921016     C* SOMMO 1 AI GIORNI ALLA DATA FOGLIO
026100921016     C     G02TGI        ADD       1             GIOTGI                          DATA AL
026200941017     C                   CALL      'XSRGI8'
026300921016     C                   PARM                    WGIDAT
026400921016     C*
026500941018     C                   Z-ADD     GIOINV        CO4DAA                          AA/MM/GG
026600941018     C                   Z-ADD     CO4DAA        WDATA                           DI COMODO
026700941018     C                   Z-ADD     CO4DAA        PA4DAA
026800941018     C                   MOVEL     GG            PA4DAA
026900941018     C                   MOVE      AA            PA4DAA                          GG/MM/AA
027000921016     C*
027100950810     C                   CLEAR                   KPJBU
027200960529     C* IMPOSTO LA KPJBU
027300941028     C                   CLEAR                   DSLR01
027400941028     C                   Z-ADD     3             DLRNPG                          CATEGORIA
027500941028     C                   MOVEL     VIDNFV        DLRNFV                          NUM FOGLIO
027600941028     C                   Z-ADD     *ZERO         DLRNPD                          PISTOLA INIZIALE
027700941028     C                   Z-ADD     *HIVAL        DLRNPA                          PISTOLA FINALE
027800941028     C                   Z-ADD     *ZERO         DLRSRD                          SERIE INIZIALE
027900941028     C                   Z-ADD     *HIVAL        DLRSRA                          SERIE FINALE
028000941028     C                   MOVEL     VIDFLF        DLRFGS                          FIL APERT F
028100941028     C                   MOVEL     'S'           DLRFS1                          STAMPA SPU
028200950810     C                   CLEAR                   KPJBU
028300941028     C                   MOVEL     DSLR01        KPJBU
028400960529     C**
028500960529     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
028600960529     C     VIDSSP        IFEQ      'S'
028700941017     C                   MOVEL     'LR06'        KCOAZ
028800920930     C                   CALL      'BCH10'
028900920930     C                   PARM                    KPJBA
029000920930     C                   END
029100971128     C*
029200971128     C                   MOVEL     'C'           D55TLA
029300971128     C                   CALL      'FNLV55R'
029400060508     C                   PARM                    fnlv55ds
029500910320     C*
029600000000     C     FINE          TAG
029700000000     C                   SETON                                        LR
029800941017     C**************************************************************************
029900941017     C* OPERAZIONI INIZIALI
030000941017     C**************************************************************************
030100920813     C     OPERAZ        BEGSR
030200920813     C*
030300020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
030400920813     C*
030500020424     C                   Z-ADD     SIMPOU        VIDFIL
030600020424     C     SIMTPP        IFEQ      '2'
030700020424     C     SIMTPP        OREQ      *BLANKS
030800020424     C                   SETON                                        05
030900020424     C                   ENDIF
031000020424     C*
031100931228     C                   MOVEL     VIDFIL        VIDFLF
031200931228     C*
031300161017     C* 11 e 01 ON - C P G
031400161007     C                   SELECT
031500161007     C                   WHEN      PARFLG='Z'
031600161007     C                   SETON                                        1101
031700161017     c* Verifico se utilizzabile per tutti o solo per la 001 e edp
031800161017     C                   MOVEL     '7B'          COD
031900161017     C                   MOVEL(P)  parflg        key
032000161017     C     KTAB          chain     TABEL
032100161017     c                   if        not %found(tabel00f)
032200161017     c                   eval      Wuin='1'
032300161017     c                   else
032400161017     c                   movel     tbluni        ds7b
032500161017     c                   eval      Wuin=§7buti
032600161017     c                   endif
032700161017     c
032800161017     C* 01 ON - I M G
032900161007    1C                   WHEN      PARFLG='G'
033000931228     C                   SETON                                        01
033100161007   x1C                   OTHER
033200050510     c* Vedo se p.o. gestione abilitato al piking messaggerie
033300050510     c     vidfil        chain     azorg01l
033400050510    2c                   if        %found(azorg01l)
033500050510     c                   movel     orgdf0        og150
033600170331     c* Verifico se fa l'IDD il giorno dopo
033700170331     c                   movel     orgde8        og148
033800050510    3c                   if        §ogpkm='S'
033900050510     c                   seton                                        02
034000050510    3C                   END
034100101215    3c                   if        §ogimaa='S'
034200101215     c                   seton                                        03
034300101215    3C                   END
034400160502     c                   if        §ogturno='S'
034500160502     c                   seton                                        06
034600160502    3C                   END
034700101215     c
034800050510    2C                   END
034900161007    1C                   ENDSL
035000931228     C*
035100931228     C* CAMPI DI PASSAGGIO
035200931228     C*
035300931228     C  N01              MOVE      TES(1)        VIDTES
035400931228     C  N01              MOVEL     TIN(1)        VIDTIN
035500161007     C  N01              MOVEL     'A'           PA5SPG
035600161007     C                   IF        *IN01
035700161007     C  N11              MOVE      TES(2)        VIDTES
035800161007     C  N11              MOVEL     TIN(2)        VIDTIN
035900161007     C  N11              MOVEL     'G'           PA5SPG
036000161007     C   11              MOVE      TES(3)        VIDTES
036100161007     C   11              MOVEL     TIN(3)        VIDTIN
036200161007     C   11              MOVEL     'Z'           PA5SPG
036300161007     C                   ENDIF
036400931228     C                   Z-ADD     3             COMNPG
036500950810     C                   MOVEL     PA5SPG        PARTIN
036600931228     C*
036700931228     C* PRENDO FASE ANOMALIA
036800931228     C*
036900950810     C                   MOVEL     'L'           PA5TLA
037000950810     C                   MOVEL     3             PA5NPG
037100950810     C                   CLEAR                   PA5FAS
037200950810     C                   CALL      'TRUL11R'
037300950810     C                   PARM                    PARAM5
037400950810     C*
037500950810     C                   MOVEL     PA5FAS        PA4FAS            1
037600931228     C*
037700931228     C*
037800931228     C                   MOVEL     *BLANKS       VIDNFV
037900931228     C                   MOVEL     'N'           VIDSN
038000960430     C                   MOVEL     'N'           VIDSSP
038100060508     c                   clear                   vidlna
038200931228     C* PER ANOMALIE
038300931228     C                   MOVEA     *BLANKS       ANM
038400931228     C                   MOVEL     'I'           PA4IE
038500931228     C                   MOVEL     ' '           PA4SN
038600931228     C                   MOVEL     ' '           PA4RIS
038700931228     C                   MOVEL     'N'           PA4STA
038800931228     C                   MOVEL     'I'           PA4INC
038900070418     C                   MOVEL     'N'           vidmdc
039000110131     C  n03              MOVEL     'N'           vidimaa
039100110131     C   03              MOVEL     'S'           vidimaa
039200941017     C*
039300920813     C                   ENDSR
039400941028      *-----------------------------------------------------***********
039500941028      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
039600941028      *-----------------------------------------------------***********
039700941028     C     CTRFGS        BEGSR
039800941028     C                   SETOFF                                       90
039900941028     C                   CLEAR                   DSLV50
040000941028     C                   MOVEL     KNMUS         D50PRU
040100941028     C                   MOVEL     VIDFIL        D50FGS
040200941028     C                   CALL      'FNLV50R'
040300941028     C                   PARM                    DSLV50
040400941028     C*
040500941028     C     D50ERR        IFNE      ' '
040600941028     C                   MOVEL     D50MSG        V1CMSG
040700941028     C                   SETON                                        499028
040800941028     C                   GOTO      ENDCTF
040900941028     C                   ENDIF
041000050510     c* Verifico se ha piking messaggerie: abilito i disabilito
041100050510     c*  il campo
041200101215     c* Verifico se ha ima per colli spuntati in arrivo: abilito o disabilito
041300160906     c                   setoff                                       020306
041400101215     c
041500170331    1c                   if        not *in01
041600050510     c     vidfil        chain     azorg01l
041700170331    2c                   if        %found(azorg01l)
041800050510     c                   movel     orgdf0        og150
041900170331     c* Verifico se fa l'IDD il giorno dopo
042000170331     c                   movel     orgde8        og148
042100101215     c
042200101215    3c                   if        §ogpkm='S'
042300101215     c                   seton                                        02
042400101215    3C                   END
042500101215     c
042600101215     c                   if        §ogimaa='S'
042700101215     c                   seton                                        03
042800110131     c                   movel     'S'           vidimaa
042900050510     C                   END
043000160906     c                   if        §ogturno='S'
043100160906     c                   seton                                        06
043200160906    3C                   END
043300170331     c
043400170331    2C                   END
043500170331    1C                   END
043600941028     C*
043700941028     C     ENDCTF        ENDSR
043800941017     C**************************************************************************
043900941017     C* CONTROLLO CAMPI
044000941017     C**************************************************************************
044100910320     C     CONTR         BEGSR
044200941017     C*
044300920624     C                   SETOFF                                       9030
044400941028     C*
044500931228     C* FILIALE DI APETTURA FOGLIO
044600941028     C                   CLEAR                   DSLV50
044700941028     C                   MOVEL     KNMUS         D50PRU
044800941028     C                   MOVEL     VIDFLF        D50FGS
044900941028     C                   CALL      'FNLV50R'
045000941028     C                   PARM                    DSLV50
045100941028     C*
045200941028     C     D50ERR        IFNE      ' '
045300941028     C                   MOVEL     D50MSG        V1CMSG
045400941028     C                   SETON                                        519028
045500941028     C                   GOTO      ENDCT1
045600941028     C                   ENDIF
045700931228     C*
045800050510     C* NUMERO FOGLIO
045900911015     C     VIDNFV        IFEQ      *BLANKS
046000911015     C     VIDNFV        OREQ      *ZEROS
046100911015     C                   SETON                                        4090
046200911015     C                   GOTO      ENDCT1
046300911015     C*
046400911015     C                   ELSE
046500911015     C* RICERCA
046600931228     C     '?'           SCAN      VIDNFV                                 90
046700931228     C   90              DO
046800941017     C                   CLEAR                   DSLR01
046900941017     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
047000941028     C                   MOVEL     'FNLR30R'     DLRPGM                         PROGRAMMA
047100941017     C                   MOVEL     '2'           DLRTFV
047200941017     C                   MOVEL     VIDFLF        DLRFGS
047300941017     C                   MOVEL     3             DLRNPG
047400941017     C                   MOVE      31129999      DLRGAL
047500941017     C                   MOVE      99991231      DLRAAL
047600941017     C                   MOVEL     DSLR01        KPJBU
047700941017     C                   CALL      'FNLR02R'
047800911015     C                   PARM                    KPJBA
047900941017     C                   MOVEL     KPJBU         DSLR01
048000941017     C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
048100941017     C                   MOVEL     DLRNFV        VIDNFV
048200941017     C                   ENDIF
048300911016     C*
048400911015     C                   GOTO      ENDCT1
048500911015     C                   END
048600911016     C*  C O N T R O L L I
048700911015     C                   TESTN                   VIDNFV               31
048800930914     C     *IN31         IFEQ      *OFF
048900930914     C                   SETON                                        30
049000930914     C*
049100930914     C                   ELSE
049200911015     C*
049300930914     C                   MOVEL     VIDNFV        COMNFV
049400941017     C     KFVV          CHAIN     FNFVV01L                           30
049500911015     C* ANNULLATO
049600911015     C  N30FVVATB        IFNE      *BLANKS
049700911015     C                   SETON                                        30
049800911015     C                   END
049900930914     C                   ENDIF
050000911016     C* ERRORE
050100911015     C   30              SETON                                        4090
050200911015     C   30              GOTO      ENDCT1
050300920813     C*
050400911015     C* CHIUSO
050500930914     C     FVVFCF        IFNE      *BLANKS
050600911015     C                   SETON                                        4290
050700911016     C                   GOTO      ENDCT1
050800911015     C                   END
050900911016     C*
051000911016     C* NON E' INVENTARIO ARRIVI
051100950810     C  N01FVVSPG        IFNE      PA5SPG
051200911016     C                   SETON                                        4190
051300911016     C                   GOTO      ENDCT1
051400911016     C                   END
051500920813     C*
051600920813     C* NON E' INVENTARIO GIACENZE
051700161007     C                   IF        *IN01
051800161007     C  N11FVVSPG        IFNE      PA5SPG
051900920813     C                   SETON                                        4390
052000920813     C                   GOTO      ENDCT1
052100920813     C                   END
052200161031     C* PER IPG SI PUÒ UTILIZZARE SIA "G" CHE "Z"
052300161007     C   11              IF        FVVSPG<>'Z' AND FVVSPG<>'G'
052400161007     C                   SETON                                        902846
052500161007     C                   MOVEA     MSG(9)        v1cmsg
052600161007     C                   GOTO      ENDCT1
052700161007     C                   END
052800161007
052900161007     C                   ENDIF
053000911015     C*
053100911015     C                   END
053200050510     c
053300050510     c* Per elaborazione definitiva, accetto solo ' ' nella scelta
053400050510     c*  delle zone di consegna
053500160502     c                   if        not *in01 and  vidsn='S'
053600160502     c                             and (*in02 or *in06)
053700160502     c                   if        vidtpm<>'  ' or vidtpm2<>'  '
053800050510     C                   SETON                                        902844
053900050510     C                   MOVEA     MSG(2)        v1cmsg
054000060508     C                   GOTO      ENDCT1
054100050510     C                   END
054200160502     C                   END
054300160502     c*
054400160502     c                   if        not *in01 and not *in06  and *in02
054500160502     c                   if        vidtpm='P1' or vidtpm='P2' or
054600160502     c                             vidtpm2='P1' or vidtpm2='P2'
054700160502     C                   SETON                                        902844
054800160502     C                   MOVEA     MSG(7)        v1cmsg
054900160502     C                   GOTO      ENDCT1
055000160502     c                   endif
055100160502     c                   endif
055200160502     c                   if        not *in01 and *in06  and not *in02
055300160502     c                   if        vidtpm='P ' or vidtpm='M ' or
055400160502     c                             vidtpm2='P ' or vidtpm2='M '
055500160502     C                   SETON                                        902844
055600160502     C                   MOVEA     MSG(7)        v1cmsg
055700160502     C                   GOTO      ENDCT1
055800160502     c                   endif
055900160502     c                   endif
056000160504     c* non posso mai accettare P +P1 o P+P2
056100160504     c                   if        vidtpm='P ' or  vidtpm2='P '
056200160504     c                   if        vidtpm='P1' or vidtpm='P2' or
056300160504     c                             vidtpm2='P1' or vidtpm2='P2'
056400160504     C                   SETON                                        902844
056500160504     C                   MOVEA     MSG(8)        v1cmsg
056600160504     C                   GOTO      ENDCT1
056700160504     c                   endif
056800160504     c                   endif
056900160502
057000060508     c* non posso selezionare linea di arrivo se elaborazione definitiva
057100060508     c                   if        vidsn='S' and vidlna>0
057200060508     C                   SETON                                        902845
057300060508     C                   MOVEA     MSG(3)        v1cmsg
057400060508     C                   GOTO      ENDCT1
057500060508     C                   END
057600101215     c* non posso selezionare solo colli spuntati alla lna finale se elab.definitiva
057700101215     c                   if        vidsn='S' and vidimaa='S'
057800101215     C                   SETON                                        902847
057900101215     C                   MOVEA     MSG(6)        v1cmsg
058000101215     C                   GOTO      ENDCT1
058100101215     C                   END
058200060508     c* Deve essere gestita dal p.o. in gestione
058300060508     c                   if        vidlna>0 and vidlna<>vidfil
058400060508     c                   clear                   fnlv55ds
058500060508     c                   eval      d55tpt='6'
058600060508     c                   eval      d55lin=vidlna
058700060508     c                   eval      d55drf=fvvdfv
058800060508     c                   call      'FNLV55R'
058900060508     c                   parm                    fnlv55ds
059000060508     c
059100060508     c                   if        d55tfa<>vidfil
059200060508     C                   SETON                                        902845
059300060508     C                   MOVEl     MSG(4)        v1cmsg
059400060508     C                   GOTO      ENDCT1
059500060508     c                   endif
059600060508     c                   endif
059700070706     c* Per elaborazione definitiva non posso elanciare in data >
059800070706     c                   if        vidsn='S' and fvvdfv>dateu
059900070706     C                   SETON                                        902846
060000070706     C                   MOVEA     MSG(5)        v1cmsg
060100070706     C                   GOTO      ENDCT1
060200070706     C                   END
060300161109     c
060400161125     c* non posso fare elaborazione definitiva per IPG con foglio IMG
060500161109     c                   if        vidsn='S' and *in11 and fvvspg='G'
060600161109     C                   SETON                                        902846
060700161109     C                   MOVEA     MSG(11)       v1cmsg
060800161109     C                   GOTO      ENDCT1
060900161109     C                   ENDif
061000910320     C*
061100911015     C     ENDCT1        ENDSR
061200941017     C**************************************************************************
061300941017     C*    R O U T I N E     I N I Z I A L E
061400941017     C**************************************************************************
061500941017     C     *INZSR        BEGSR
061600941017     C*
061700941017     C     KFVV          KLIST
061800941017     C                   KFLD                    COMNPG
061900941017     C                   KFLD                    COMNFV
062000941028     C                   KFLD                    VIDFLF
062100161017     C     KTAB          KLIST
062200161017     C                   KFLD                    CODUT
062300161017     C                   KFLD                    COD               2
062400161017     C                   KFLD                    KEY               8
062500941017     C*
062600941017     C     *LIKE         DEFINE    FVVNPG        COMNPG                         CATEGORIA FOGLIO
062700941017     C     *LIKE         DEFINE    FVVNFV        COMNFV                         NR.FOGLIO
062800961017     C     *LIKE         DEFINE    VIDNFV        WNFV
062900971128     C     *LIKE         DEFINE    D55TFA        WFE0
063000971128     C     *LIKE         DEFINE    D55TFP        WTFP
063100070706     c* data del giorno
063200070706     C                   TIME                    W0140            14 0
063300070706     C* UDATE IN GGMMAAAA
063400070706     C                   MOVE      W0140         WDTGIO            8 0
063500070706     C*
063600070706     C* UDATE IN AAAAMMGG
063700070706     C                   Z-ADD     WDTGIO        G02DAT
063800070706     C                   MOVEL     *BLANK        G02ERR
063900070706     C                   CALL      'XSRDA8'
064000070706     C                   PARM                    WLBDAT
064100070706     C                   MOVEL     G02INV        DATEU             8 0
064200941017     C*
064300941017     C                   ENDSR
064400941017     C**************************************************************************
064500961016**
064600911018 ** INVENTARIO  MAGAZZINO  ARRIVI **
064700920813 ** INVENTARIO MAGAZZINO GIACENZE **
064800161007 * QUADRATURA COLLI A MAG.GIACENZE *
064900911018**
065000911018 I . M . A .
065100920813 I . M . G .
065200161031 I . P . G .
065300961016**  MSG
065400101215Manca l'elaboraz. dell'IDA del proprio terminal di arr. xxx. ENTER PER FORZARE     1
065500160504Per elaborazione DEFINITIVA non si può scegliere il tipo zona consegna             2
065600101215Per elaborazione DEFINITIVA non si può selezionare la linea di arrivo              3
065700101215Linea di Arrivo errata: non in gestione al P.O. selezionato                        4
065800101215Impossibile elaborazione definitiva per foglio in data MAGGIORE di oggi            5
065900101215Per elaborazione DEFINITIVA non selezionare solo colli spuntati alla LNA finale    6
066000160502Tipo scelta zone di consegna non prevista                                          7
066100160504Scelta tipi  zone di consegna non corretta                                         8
066200161017Il foglio per la quadratura colli a Mag.Giac. deve essere un tipo "Z" o "G"        9
066300161125Funzione abilitata soltanto in TEST per alcune filiali                            10
066400161110Impossibile chiudere foglio IMG dalla quadratura colli a mag.giacenze (IPG)       11
