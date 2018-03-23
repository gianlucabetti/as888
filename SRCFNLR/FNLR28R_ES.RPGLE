000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020419      * FNLR28R *----------------------------------------------------*
000300020419      *         - FILTRO LANCIO IDA
000400020419      *--------------------------------------------------------------*
000500941222     FFNLR28D   CF   E             WORKSTN
000600140619     fazcpc03l  if   e           k disk
000700941222     FFNFVV01L  IF   E           K DISK
000800140619     FFNfva02L  IF   E           K DISK
000900930121     FTABEL00F  IF   E           K DISK
001000941222     D*
001100911015     D SKI             S              1    DIM(5)                               SKI DI UNO
001200930121     D L1              S              3  0 DIM(30)                              FIL GESTITE
001300930121     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
001400930121     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
001500140612
001600140612     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001700020419
001800020419     d wa              s              2  0
001900020419     d wb              s              2  0
002000020419     d wc              s              2  0
002100020419
002200941222     D*
002300941222     D DSLR01        E DS
002400930205     D DS7N          E DS
002500920831     D KPJBA         E DS
002600101202      *
002700101202      * - Parametri per richiamo al pgm di controllo profilo utenti
002800101202     d TIBS34ds      e ds
002900101202      * - Ds di riferimento al file esterno AzUte00F
003000101202     d AZUTEds       e ds                  ExtName(AzUte00F)
003100101202      * - Ds per dati organigramma
003200101202     d dDatiUte      e ds
003300140619
003400140619     D tnsd99ds      E DS
003500101202      *
003600070706     D WLBDAT          DS
003700070706     D  G02DAT                 1      8  0
003800070706     D  G02INV                 9     16  0
003900070706     D  G02ERR                17     17
004000070706     D  G02TGI                18     22  0
004100020419      * ds per trul06r
004200020419     d trul06ds      e ds
004300020419     d  lin                    1     90  0 dim(30)
004400140612
004500140612     d  savnfv         s                   like(vidnfv)
004600140619     d  wcad           s                   like(cpccap)
004700020419
004800900702     C****************************************************************
004900900702     C*  RIEPILOGO INDICATORI
005000900702     C***************************************************************
005100920715     C* 40/42 - ERRORE
005200900702     C* 31/32 - DI COMODO
005300900702     C*****************************************************************
005400000000     C     *ENTRY        PLIST
005500000000     C                   PARM                    KPJBA
005600941222     C*
005700900511     C*---------------------------------------------------------------*
005800910320     C     KFVV          KLIST
005900911015     C                   KFLD                    COMNPG            1 0
006000911015     C                   KFLD                    COMNFV            5 0
006100941222     C                   KFLD                    VIDFGS
006200140612     C     KFVV3         KLIST
006300140619     C                   KFLD                    knpg              1 0
006400140619     C                   KFLD                    knfa              5 0
006500140619     C                   KFLD                    VIDFGS
006600140619     c                   clear                   knfa
006700140619     c                   clear                   kpgg
006800930121     C     KTAB2         KLIST
006900930121     C                   KFLD                    CODUT
007000930121     C                   KFLD                    COD               2
007100930121     C                   KFLD                    KEY               8
007200930121     C*
007300930121     C                   EXSR      OPERAZ
007400911015     C*
007500911015     C                   MOVEL     *BLANKS       VIDNFV
007600911015     C                   MOVEL     'N'           VIDSN
007700920930     C                   MOVEL     'S'           VIDSSP
007800101202     C*
007900040227     C                   MOVEL     '2'           VIDNPG
008000040227     C*
008100900509     C     FOR01         TAG
008200941222     C                   EXFMT     LR28D01
008300140612     c                   setoff                                       2890
008400140612     c
008500140612     C** F3 - FINE LAVORO
008600900509     C   KC              GOTO      FINE
008700140612     C** F18- CAMBIO FILIALE
008800930121     C   KS              SETON                                        04
008900930121     C   KS              GOTO      FOR01
009000930121     C*
009100941222     C   04VIDFGS        IFNE      SIMFEL
009200941222     C     VIDFGS        LOOKUP    L1                                     30
009300930121     C  N30              SETON                                        50
009400930121     C  N30              GOTO      FOR01
009500930121     C*
009600930121     C* NON POSSO CAMBIARE SE E' IN MIA L6
009700941222     C     VIDFGS        LOOKUP    L6S                                    49
009800930121     C   49              GOTO      FOR01
009900930121     C                   END
010000930121     C*
010100930205     C   04              EXSR      CARL6
010200910801     C*  CONTROLLI
010300910801     C                   EXSR      CONTR
010400941222     C  NKF
010500910801     COR 90              GOTO      FOR01
010600941222     C* F6 - ELABORAZIONE
010700941228     C                   CLEAR                   KPJBU
010800941228     C                   CLEAR                   DSLR01
010900941228     C                   Z-ADD     VIDFGS        DLRFGS
011000941228     C                   MOVEL     VIDNPG        DLRNPG
011100941228     C                   MOVEL     VIDNFV        DLRNFV
011200941228     C                   MOVEL     VIDSN         DLRFS1
011300941228     C                   MOVEL     DSLR01        KPJBU
011400950817     C                   MOVEL     'LR32'        KCOAZ
011500900702     C                   CALL      'BCH10'
011600900702     C                   PARM                    KPJBA
011700920930     C*
011800920930     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
011900920930     C     VIDSSP        IFEQ      'S'
012000941227     C                   CLEAR                   DSLR01
012100941227     C                   Z-ADD     VIDFGS        DLRFGS
012200941227     C                   MOVEL     VIDNPG        DLRNPG
012300941227     C                   MOVEL     VIDNFV        DLRNFV
012400941228     C                   MOVEL     'S'           DLRFS1                         'S'=STAMPA SPUNTE
012500941228     C                   Z-ADD     *HIVAL        DLRNPA                         NR.PISTOLA FINALE
012600941228     C                   Z-ADD     *HIVAL        DLRSRA                         SERIE FINALE
012700941227     C                   MOVEL     DSLR01        KPJBU
012800941222     C                   MOVEL     'LR06'        KCOAZ
012900920930     C                   CALL      'BCH10'
013000920930     C                   PARM                    KPJBA
013100920930     C                   END
013200910320     C*
013300000000     C     FINE          TAG
013400000000     C                   SETON                                        LR
013500941222     C**************************************************************************
013600941222     C* OPERAZIONI INIZIALI
013700941222     C**************************************************************************
013800930121     C     OPERAZ        BEGSR
013900101202      *
014000101202      * impostazione campi fissi
014100101202     C                   Z-ADD     1             CODUT             1 0
014200101202      *
014300101202      * Reperimento dati utente/aziendali
014400101202     c                   exsr      sr_DatiJob
014500930121     C*
014600070706     C* VEDO SE SONO 1° o 2° livello
014700930121     C*
014800101202     c     dutLPO        ifeq      '2'
014900101202     c     dutLPO        oreq      *blanks
015000101202     c                   movel     dutPOU        vidfgs
015100930121     C                   SETON                                        05
015200930205     C                   ELSE
015300941222     C                   MOVEL     SIMFEL        VIDFGS
015400930121     C                   END
015500930205     C*
015600930205     C                   EXSR      CARL6
015700930121     C*
015800020419     C* CARICO FILIALI GESTITE £1
015900930121     C*
016000020419     c                   clear                   trul06ds
016100020419     c                   eval      d06cod = '£1'
016200020419     c                   movel     simfel        d06key
016300020419     c                   movel     trul06ds      kpjbu
016400020419     c                   call      'TRUL06R'
016500020419     c                   parm                    kpjba
016600020419     c                   movel     kpjbu         trul06ds
016700020419     c                   movea     lin           l1
016800020419
016900020419     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE
017000020419     c                   clear                   wc
017100020419 b1  c                   do        30            wa
017200020422     c                   if        l1(wa) = *zeros
017300020422     c                   leave
017400020422     c                   endif
017500020419     c                   clear                   kpjbu
017600020419     c                   clear                   trul06ds
017700020419     c                   eval      d06cod = '£6'
017800020419     c                   movel     l1(wa)        d06key
017900020419     c                   eval      d06esc = 'S'
018000020419     c                   movel     trul06ds      kpjbu
018100020419     c                   call      'TRUL06R'
018200020419     c                   parm                    kpjba
018300020419     c                   movel     kpjbu         trul06ds
018400020419 b2  c                   do        30            wb
018500020419     c                   if        lin(wb) = *zeros
018600020419     c                   leave
018700020422     c                   endif
018800020419     c                   add       1             wc
018900020419     c                   z-add     lin(wb)       l6s(wc)
019000020419 e2  c                   enddo
019100020419 e1  c                   enddo
019200070706     c* data del giorno
019300070706     C                   TIME                    W0140            14 0
019400070706     C* UDATE IN GGMMAAAA
019500070706     C                   MOVE      W0140         WDTGIO            8 0
019600070706     C*
019700070706     C* UDATE IN AAAAMMGG
019800070706     C                   Z-ADD     WDTGIO        G02DAT
019900070706     C                   MOVEL     *BLANK        G02ERR
020000070706     C                   CALL      'XSRDA8'
020100070706     C                   PARM                    WLBDAT
020200070706     C                   MOVEL     G02INV        DATEU             8 0
020300070706     C*
020400020419
020500941222     C*
020600930121     C                   ENDSR
020700941222     C**************************************************************************
020800941222     C* CARICO L6
020900941222     C**************************************************************************
021000930121     C     CARL6         BEGSR
021100941222     C*
021200020419     c                   clear                   trul06ds
021300020419     c                   eval      d06cod = '£6'
021400020419     c                   movel     vidfgs        d06key
021500020419     c                   movel     trul06ds      kpjbu
021600020419     c                   call      'TRUL06R'
021700020419     c                   parm                    kpjba
021800020419     c                   movel     kpjbu         trul06ds
021900020419     c                   movea     lin           l6
022000941222     C*
022100930121     C                   ENDSR
022200941222     C**************************************************************************
022300941222     C* CONTROLLO CAMPI
022400941222     C**************************************************************************
022500910320     C     CONTR         BEGSR
022600941222     C*
022700920624     C                   SETOFF                                       9030
022800930205     C* CATEGORIA FOGLI
022900930205     C     VIDNPG        IFEQ      ' '
023000930205     C                   SETON                                        5190
023100930205     C                   GOTO      ENDCT1
023200930205     C                   END
023300930205     C     VIDNPG        IFEQ      '?'
023400930205     C                   MOVEL     CODUT         §KUT
023500930205     C                   MOVEL     '7N'          §COD
023600930205     C                   MOVE      ' '           VIDNPG
023700930205     C                   MOVEL     *BLANKS       §KEY
023800930205     C                   CALL      'X§TABER'
023900930205     C                   PARM                    §KUT              1 0
024000930205     C                   PARM                    §COD              2
024100930205     C                   PARM                    §KEY              8
024200930205     C                   PARM                    §DES             30
024300930205     C                   MOVEL     §KEY          VIDNPG
024400930205     C                   SETON                                        90
024500930205     C                   GOTO      ENDCT1
024600930205     C                   END
024700930205     C* CONTROLLO
024800930205     C                   MOVEL     '7N'          COD
024900930205     C                   MOVEL     *BLANKS       KEY
025000930205     C                   MOVEL     VIDNPG        KEY
025100930205     C* INESISTENTE
025200930205     C     KTAB2         CHAIN     TABEL                              51
025300930205     C* ANNULLATA
025400930205     C  N51TBLFLG        IFNE      ' '
025500930205     C                   SETON                                        51
025600930205     C                   ELSE
025700930205     C     VIDNPG        IFNE      '2'
025800930205     C                   SETON                                        51
025900930205     C                   END
026000930205     C                   END
026100930205     C* ERRATA
026200930205     C   51              SETON                                        90
026300930205     C   51              GOTO      ENDCT1
026400930205     C*
026500930205     C                   MOVEL     TBLUNI        DS7N
026600930205     C* NON UTILIZZABILE DALLA FILIALE IN GESTIONE
026700930205     C     §7NUTI        IFEQ      '9'
026800930205     C                   SETON                                        5290
026900930205     C                   GOTO      ENDCT1
027000930205     C*
027100930205     C                   ELSE
027200930205     C*
027300930205     C* SOLO DALLA PRIMO LIVELLO UTILIZZABILE
027400930205     C     §7NUTI        IFEQ      '1'
027500941222     C     VIDFGS        ANDNE     SIMFEL
027600930205     C                   SETON                                        5290
027700930205     C                   GOTO      ENDCT1
027800930205     C                   END
027900930205     C     §7NUTI        IFEQ      '2'
028000941222     C     VIDFGS        ANDEQ     SIMFEL
028100930205     C                   SETON                                        5290
028200930205     C                   GOTO      ENDCT1
028300930205     C                   END
028400930205     C                   END
028500930205     C                   MOVEL     VIDNPG        COMNPG            1 0
028600930205     C*
028700911015     C* NUMERO FOGLIO VIAGGIO
028800911015     C     VIDNFV        IFEQ      *BLANKS
028900911015     C     VIDNFV        OREQ      *ZEROS
029000911015     C                   SETON                                        4090
029100911015     C                   GOTO      ENDCT1
029200911015     C*
029300911015     C                   ELSE
029400911015     C*
029500911015     C                   MOVEA     VIDNFV        SKI
029600911015     C* RICERCA
029700911015     C     '?'           LOOKUP    SKI                                    09
029800911015     C   09              DO
029900941222     C                   CLEAR                   DSLR01
030000941222     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
030100941222     C                   MOVEL     VIDNPG        DLRNPG                         CATEGORIA FOGLIO
030200941222     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
030300941222     C                   MOVEL     'FNLR28R'     DLRPGM                         NOME PROGRAMMA
030400941222     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
030500941222     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
030600941222     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
030700941222     C                   MOVEL     DSLR01        KPJBU
030800941222     C                   CALL      'FNLR02R'
030900911015     C                   PARM                    KPJBA
031000941222     C                   MOVEL     KPJBU         DSLR01
031100941222     C                   MOVEL     DLRNFV        VIDNFV
031200911015     C                   SETON                                        90
031300911015     C                   GOTO      ENDCT1
031400911015     C                   END
031500911015     C* CONTROLLI
031600911015     C                   TESTN                   VIDNFV               31
031700911015     C  N31              SETON                                        30
031800911015     C*
031900911015     C  N30              MOVEL     VIDNFV        COMNFV
032000941222     C  N30KFVV          CHAIN     FNFVV01L                           30
032100911015     C* ANNULLATO
032200911015     C  N30FVVATB        IFNE      *BLANKS
032300911015     C                   SETON                                        30
032400911015     C                   END
032500911015     C   30              SETON                                        4090
032600911015     C   30              GOTO      ENDCT1
032700930121     C*
032800930121     C* INGESTIONE AD ALTRA FILIALE
032900941222     C     FVVFGS        LOOKUP    L6                                     30
033000930121     C  N30              SETON                                        4890
033100930121     C  N30              GOTO      ENDCT1
033200911015     C* CHIUSO
033300911015     C  N30FVVFCF        IFNE      *BLANKS
033400911015     C                   SETON                                        4290
033500911015     C                   END
033600070706     c*
033700070706     c* non posso chiudere un foglio in data di domani
033800070706     c                   if        vidsn='S' and fvvdfv>dateu
033900070706     C                   SETON                                        5390
034000070706     c                   endif
034100140612     c                   if        vidnfv<>savnfv
034200140612     c                   clear                   for_1             1
034300140612     c                   movel     vidnfv        savnfv
034400140612     c                   endif
034500911015     C*
034600140612     c* rispetto alla data foglio da chiudere, vedo se ci sono dei fogli ancora aperti
034700140612     c                   if        for_1=' '
034800140619     c* prendo la versione cappario con la data foglio arrivi
034900140619     C                   Z-ADD     fvvdfv        P96DRI            8 0
035000140619     C                   Z-ADD     *ZEROS        P96VER            5 0
035100140619     C                   CALL      'TISI96R'
035200140619     C                   PARM                    P96DRI
035300140619     C                   PARM                    P96VER
035400140619     c* prendo una CAP della fil di arrivo
035500140619     c                   clear                   cpccap
035600140619     c     kcpc          setll     azcpc03l
035700140619     c     kcpc          reade     azcpc03l
035800140619     c                   dow       not %eof(azcpc03l)
035900140619     c                   if        cpcatb<>' ' or cpcfit='S'
036000140619     c                   clear                   cpccap
036100140619     c     kcpc          reade     azcpc03l
036200140619     c                   else
036300140619     c                   leave
036400140619     c                   endif
036500140619     c                   enddo
036600140619     c
036700140619     c                   if        cpccap=*blanks
036800140619     c     vidfgs        chain     azorg01l
036900140619     c                   eval      wcad=%editc(orgcpf:'X')
037000140619     c                   else
037100140619     c                   eval      wcad=cpccap
037200140619     c                   endif
037300140612     c
037400140619     c     kfva          setll     fnfva02l
037500140619     c     kfva          reade     fnfva02l
037600140619     c                   dow       not %eof(fnfva02l)
037700140619     c* se ci sono fogli
037800140619     c                   clear                   tnsd99ds
037900140619     c                   eval      d98tla='L'
038000140619     c                   eval      d98lnp=fvalnp
038100140619     c                   eval      d98tfp=fvalnp
038200140619     c                   eval      d98lna=vidfgs
038300140619     c                   eval      d98tfa=vidfgs
038400140619     c                   eval      d98tsp='E'
038500140619     c                   eval      d98cap=wcad
038600140619     c                   call      'TNSD99R'
038700140619     c                   parm                    tnsd99ds
038800140619
038900140612     c                   enddo
039000140612     c
039100140612     c                   if        not %eof(fnfvv03l)
039200140612     c                   seton                                        2890
039300140612     c                   eval      vidmsg=msg(1)
039400140612     c                   eval      %subst(vidmsg:18:5)=%editc(ol_nfv:'X')
039500140612     C* UDATE IN AAAAMMGG
039600140612     C                   Z-ADD     ol_DFV        G02inv
039700140612     C                   MOVEL     3             G02ERR
039800140612     C                   CALL      'XSRDA8'
039900140612     C                   PARM                    WLBDAT
040000140612     c                   eval      %subst(vidmsg:28:10)=
040100140612     c                             %editw(g02dat:'  /  /    ')
040200140612
040300140612     c                   eval      for_1='F'
040400140612     c                   endif
040500140612     c                   endif
040600140612     c
040700920713     C                   END
040800910320     C*
040900911015     C     ENDCT1        ENDSR
041000101202
041100101202      *---------------------------------------------------------------*
041200101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
041300101202      *---------------------------------------------------------------*
041400101202     c     sr_DatiJob    BEGSR
041500101202      *
041600101202     c     *dtaara       define    §azute        AZUTEds
041700101202     c     *dtaara       define    §datiute      dDatiUte
041800101202      *
041900101202     c                   in(E)     *dtaara
042000101202     c                   IF        %Error or RSUT = *blanks
042100101202     c                   clear                   TIBS34ds
042200101202     c                   call      'TIBS34R'
042300101202     c                   parm                    TIBS34ds
042400101202     c                   in        *dtaara
042500101202     c                   ENDIF
042600101202      *
042700101202     c                   ENDSR
042800140612**
042900140612Il foglio arrivi xxxxx del xx/xx/xxxx non è ancora stato chiuso! Enter forza
