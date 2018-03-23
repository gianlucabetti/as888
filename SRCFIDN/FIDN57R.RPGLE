000100980506      *------------------------------------------------------------------------*
000200990706      * GESTIONE raggruppamenti tipi responsabilità                            *
000300980506      *------------------------------------------------------------------------*
000400980506     H DECEDIT('0,') DATEDIT(*DMY.)
000500980506      *--------------------------------------------
000600990706     FFIDN57D   CF   E             WORKSTN sfile(fi57s03:nrr1)
000700990719     F                                     infds ($Infds)
000800990916     Ffndrr01l  uf a e           k disk
000900990916     FTNTBE01L  uf a E           k DISK
001000990722     Fsysprt    O    F  132        PRINTER       oflind(*INOF)   usropn
001100980506      *--------------------------------------------
001200990924     D savnrr          S                   LIKE(nrr1)
001300990927     D saverr          S                   LIKE(nrr1)
001400990927     D watb            S                   LIKE(tbeatb)
001500990927     D codt            S                   LIKE(tbecod)
001600990719     D kcodt           S                   LIKE(tbecod)
001700990719     D kkey1           S                   LIKE(tbeke1)
001800990719     D kkey2           S                   LIKE(tbeke2)
001900990722     D s3cfl           S                   LIKE(v3cfl)
002000990719     D s3dcfl          S                   LIKE(v3dcfl)
002100990722     D s3dtre          S             35
002200990719     D s3drta          S                   LIKE(v3drta)
002300990706      *
002400990127     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
002500990706     d cfl             s              2    dim(100)
002600990706     d cfld            s             30    dim(100)
002700990706     d tre             s              4    dim(100)
002800990722     d tred            s             35    dim(100)
002900990916     d new             s              1    dim(100) inz('2')
003000990916     d rta             s              1    dim(100)
003100980507      *--------------------------------------------
003200980507     D KPJBA         E DS
003300990706     D CNCR80        E DS
003400990706     D UT§DSE0F      E DS
003500990927     d TIBS02DS      E DS
003600990706     D dtreds        E DS                  extname(dtre)
003700990719     d$infds           ds
003800990729     d  $xrn01               378    379b 0
003900980908     C*  RIEPILOGO INDICATORI
004000980908      *--------------------------------------------
004100980908      * 20/22 - Gestione SUBFILE
004200980908      * 28    - ERRORE GENERICO EMISSIONE CAMPO MESSAGGI
004300980908      * 30/34 - di comodo
004400990127      * 40/42 - Errori a video
004500980908      *----------------------------------------------------------
004600980506      *
004700980506      * EMISSIONE PRIMA VIDEATA
004800980506     C     EMD01         TAG
004900990706     c                   move      '1'           v1sce
005000990706     C                   EXFMT     FI57D01
005100980506     C                   CLEAR                   V1CMSG
005200980506     C                   eval      *in28 = *OFF
005300980506      * F3=FINE
005400981030     c   KC              goto      fine
005500980506      * F8=STAMPA
005600980508     c                   IF        *INKH =*on
005700980508     c                   exsr      SUB_STAMPA
005800990927     C                   movel     MSG(2)        V1CMSG
005900980508     C                   eval      *in28 = *on
006000980506     c                   goto      EMD01
006100980508     c                   ENDIF
006200990924     C
006300990924      * EMISSIONE SECONDA VIDEATA                                           --
006400990924      *
006500990924      * pulizia del subfile dettaglio anagrafica fase
006600990924     c     emd02         tag
006700990924     c                   clear                   fi57s03
006800990924     c                   eval      *IN22 = *ON
006900990924     c                   WRITE     fi57c03
007000990924     c                   setoff                                       202122
007100990924     c                   clear                   nrr1
007200990924     c                   clear                   $xrn01
007300990924     C
007400980717      * gestione subfile tipo anomalie valide
007500990924     c                   exsr      carsbf
007600990924      *
007700990924     c                   exsr      gessbf
007800990927     c
007900990927      * F12=RITORNO  o dopo aggiornamento ritorno a 1 videata
008000990927     C  nkc              GOTO      EMD01
008100990924      * F 3-FINE
008200990924     B*
008300980506     C     FINE          TAG
008400980506     C                   EVAL      *INLR = *ON
008500980717      *****************************************************************
008600980717      *   ROUTINE DI CARICAMENTI SUBFILE
008700980717      *****************************************************************
008800990706     C     carsbf        begsr
008900980717      * caricamento subfile dei tipi anomalie
009000980717     c                   z-add     *zeros        nrr1              4 0
009100990927     C                   movel     kkey1         wrfl
009200990924     C     WRFL          setll     fndrr01l
009300990927     c
009400990706     c                   do        *hival
009500990706     c                   read      fndrr01l                               31
009600990706     c   31              leave
009700990927     c
009800990927     c                   z-add     1             i
009900990927     c     drrtre        lookup    tre(i)                                 66
010000990927     c* se non trovo cod.filtro , non lo carico e cancello il record in DRR
010100990927     c     *in66         ifeq      *off
010200990927     c                   movel     'A'           drratb
010300990927     c                   clear                   drrftr
010400990927     c                   update    fndrr000
010500990927     c                   else
010600990927     c   66              movel(p)  tred(i)       v3dtre
010700990927     c   66              move      *blank        new(i)
010800990927     c
010900990706     c                   move      drrrfl        v3rfl
011000990706     c                   move      drrcfl        v3cfl
011100990709     c                   move      drrcfl        v3cflh
011200990709     c                   move      drrtre        v3treh
011300990709     c                   move      drrrta        v3rtah
011400990927     c                   move      drrtre        v3tre
011500990706     c                   z-add     1             i
011600990706     c     v3cfl         lookup    cfl(i)                                 66
011700990706     c   66              movel(p)  cfld(i)       v3dcfl
011800990929     c  n66              movel     '????????????'v3dcfl
011900990929     c  n66              seton                                        27
012000990927     c                   exsr      scrivi
012100990927     c                   endif
012200980717     c                   enddo
012300990916      * record presenti solo in tabella e non nel file
012400990916      *
012500990916     c                   clear                   fi57s03
012600990916     c                   do        100           b                 3 0
012700990916     c                   if        new(b) = '1'
012800990916     c                   move      *blank        v3cflh
012900990916     c                   move      tre(b)        v3treh
013000990916     c                   movel(p)  tre(b)        v3tre
013100990916     c                   movel(p)  tred(b)       v3dtre
013200990927     c                   movel(p)  rta(b)        v3rtah
013300990916     c                   exsr      scrivi
013400990916     c                   else
013500990916     c     new(b)        comp      '2'                                    88
013600990916     c   88              leave
013700990916     c                   endif
013800990924     C
013900990916     c                   enddo
014000990730     c                   z-add     1             nrr1
014100980717      *
014200980717     c                   endsr
014300990916     c*--------------------------------------------------------
014400990916     c     scrivi        begsr
014500990916     c*--------------------------------------------------------
014600990927     c                   clear                   v3drta
014700990916     c                   select
014800990924     c                   when      v3rtah= 'A'
014900990916     c                   move      'AMMANCO '    v3drta
015000990924     c                   when      v3rtah= 'M'
015100990916     c                   move      'MANCANZA'    v3drta
015200990924     c                   when      v3rtah= 'V'
015300990916     c                   move      'AVARIA  '    v3drta
015400990924     c                   when      v3rtah= ' '
015500990916     c                   move      'GENERICO'    v3drta
015600990916     c                   endsl
015700990916     c                   add       1             nrr1
015800990916     c                   write     fi57s03
015900990929     c                   setoff                                       27
016000990916     c                   endsr
016100980717      *****************************************************************
016200980717      *   ROUTINE DI gestione del subfile
016300980717      *****************************************************************
016400980717     C     GESSBF        BEGSR
016500990924     C*
016600980717     c     nrr1          comp      *zeros                             2020
016700980717     c                   seton                                        21
016800990924     c
016900990706     c                   write     fi57z03
017000990924     C
017100990709     c                   do        *hival
017200990924     c* c'e' errore - emetto videata dell'errore
017300990927     c     saverr        ifgt      0
017400990927     c                   z-add     saverr        $xrn01
017500990927     c                   else
017600990927     c     savnrr        ifgt      0
017700990927     c                   z-add     savnrr        $xrn01
017800990927     c                   endif
017900990927     c                   endif
018000990927     c                   clear                   saverr
018100990927     c                   clear                   savnrr
018200990924     c
018300990924     c* se non ce' posizionamento cursore, lo imposto
018400990924     c     $xrn01        ifeq      0
018500990924     c                   z-add     1             $xrn01
018600990924     c                   endif
018700990924     c
018800990709     c                   exfmt     fi57c03
018900990927     c                   clear                   v1cmsg
019000990927     c                   setoff                                       282990
019100990924     C* F 3 - FINE
019200990924     c   kC              MOVEL     '1'           WFINE             1
019300990927     c   kC              goto      endsbf
019400990924     C* F12 - RITORNO
019500990927     c   kl              goto      endsbf
019600990924     C*
019700990927     c                   z-add     1             nrr1
019800990927     c                   do        *hival
019900990927     c                   readc     fi57s03                                32
020000990726     c   32              leave
020100990927     c                   setoff                                       282927
020200990927     c                   clear                   wricerca
020300990924     C*
020400990927     c     '?'           scan      v3cfl                                  27
020500990927     c                   if        *in27
020600990927     C                   MOVEL     'R'           t02mod
020700990927     C                   MOVEL     knsif         t02sif
020800990927     C                   MOVEL     'RRD'         t02cod
020900990927      *
021000990927     C                   CALL      'TIBS02R'
021100990927     C                   PARM                    KPJBA
021200990927     C                   PARM                    TIBS02DS
021300990927      *
021400990927     C                   IF        T02err = *BLANKS
021500990927     C                   movel     T02uni        v3dcfl
021600990927     C                   movel     T02ke2        v3cfl
021700990927     c                   movel     'S'           wricerca          1
021800990927     c                   z-add     nrr1          saverr
021900990927     c                   else
022000990927     c                   clear                   v3dcfl
022100990927     c                   movel     v3cflh        v3cfl
022200990927     c                   endif
022300990927     c
022400990927     c                   write     fi57z03
022500990927     c                   endif
022600990927     c
022700990726     c                   z-add     1             i
022800990927     c     v3cfl         lookup    cfl(i)                                 32
022900990924     C*
023000990927     c                   if        not *in32  OR V3CFL=*BLANKS
023100990927     c                   seton                                        282927
023200990927     c                   if        saverr=0
023300990927     c                   z-add     nrr1          saverr
023400990927     c                   endif
023500990726     c                   movel     msg(1)        v1cmsg
023600990927     c                   else
023700990927     c                   movel     cfld(i)       v3dcfl
023800990927     c
023900990927     c                   if        saverr=0
024000990927     c                   z-add     nrr1          savnrr
024100990726     c                   endif
024200990927     c                   endif
024300990927     c
024400990927     c                   update    fi57s03
024500990927     c     wricerca      ifeq      's'
024600990927     c                   leave
024700990927     c                   endif
024800990726     c                   enddo
024900990927     c
025000990927     c                   if        saverr=0
025100990927     c                   z-add     1             nrr1
025200990927     c     nrr1          chain     fi57s03                            32
025300990927     c     *in32         doweq     *off
025400990927     c                   if        V3CFL=*BLANKS
025500990927     c                   seton                                        2829
025600990927     c
025700990927     c                   if        saverr=0
025800990927     c                   z-add     nrr1          saverr
025900990927     c                   endif
026000990927     c
026100990927     c                   movel     msg(1)        v1cmsg
026200990927     c                   update    fi57s03
026300990927     c                   endif
026400990927     c
026500990927     c                   add       1             nrr1
026600990927     c     nrr1          chain     fi57s03                            32
026700990927     c                   enddo
026800990927     c                   endif
026900990924     C
027000990924     C* F6 - CONFERMA
027100990927     c   kf              if        saverr=0
027200990927     C                   EXSR      AGGIORNA
027300990927     c                   LEAVE
027400990927     c                   endif
027500990927     c
027600990927     C* F7 -gestione raggruppamento responsabilita'
027700990927     c                   if        *inkg
027800990927     c                   exsr      gesrag
027900990927     c                   iter
028000990927     c                   endif
028100990927     c
028200990709     c                   enddo
028300980717      *
028400990927     c     endsbf        endsr
028500980717      *
028600990924     c*---------------------------------------------------------------------
028700990927     c* Aggiorna file FNDRR00F
028800990924     c*---------------------------------------------------------------------
028900990924     c     aggiorna      begsr
029000990924     c
029100990924     c                   z-add     1             nrr1
029200990924     c     nrr1          chain     fi57s03                            30
029300990924     c     *in30         doweq     *off
029400990924     c*
029500990924     c     v3cfl         ifne      *blanks
029600991001     c                   movel     kkey1         wrfl
029700990924     c                   move      v3cflh        wcfl
029800990924     c                   move      v3treh        wtre
029900990924     c                   move      v3rtah        wrta
030000990924     c     kdrr          chain     fndrr01l                           66
030100990927     c* scrivo se record nuovo
030200990927     c     *in66         ifeq      *on
030300990927     c*        se cambiato codice filtro
030400990927     c     drrcfl        orne      v3cfl
030500990927     c*        se era annullato
030600990927     c     drratb        orne      ' '
030700990927     c   66              clear                   fndrr000
030800990924     c                   move      v3cfl         drrcfl
030900990924     c                   move      *blank        drrftr
031000990924     c   66              move      v3tre         drrtre
031100990924     c   66              move      v3rtah        drrrta
031200991001     c   66              movel     kkey1         drrrfl
031300990927     c
031400990924     c   66              write     fndrr000
031500990927     c  n66              update    fndrr000
031600990927     c                   endif
031700990927     c
031800990924     c                   move      *blank        wcfl
031900990924     c                   move      *blank        wtre
032000990924     c                   move      *blank        wrta
032100990924     c                   endif
032200990924     c
032300990924     c                   add       1             nrr1
032400990927     c     nrr1          chain     fi57s03                            30
032500990924     c                   enddo
032600990924     c
032700990924     c                   endsr
032800990719     c*--------------------------------------------------
032900990927     c* gestione CODICE RAGGRUPPAMENTO RESPONSABILITA'
033000990719     c*--------------------------------------------------
033100990927     c     gesrag        begsr
033200990927     c
033300990927     c                   clear                   v5msg
033400990927     c                   clear                   v5cfl
033500990927     c                   clear                   wesci             1
033600990927     c
033700990927    1c                   dow       wesci=*blanks
033800990927     c                   exfmt     fi57w05
033900990927     c                   clear                   v5msg
034000990927     c                   setoff                                       01
034100990927     c* f12 - ritorno
034200990927    2c                   select
034300990927    2c                   when      *inkl
034400990927     c                   movel     '1'           wesci
034500990927     c* f10 - inserimento
034600990927    2c                   when      *inkj
034700990927     c                   exsr      insrag
034800990927     c                   movel     '1'           wesci
034900990927     c
035000990927   x2c                   other
035100990927     c
035200990927     c*       Variazione
035300990927    3c                   if        v5cfl<>*blanks
035400990927     c     '?'           scan      v5cfl                                  32
035500990927     c                   z-add     1             i
035600990927     c                   if        *in32
035700990927     C                   MOVEL     'R'           t02mod
035800990927     C                   MOVEL     knsif         t02sif
035900990927     C                   MOVEL     'RRD'         t02cod
036000990927      *
036100990927     C                   CALL      'TIBS02R'
036200990927     C                   PARM                    KPJBA
036300990927     C                   PARM                    TIBS02DS
036400990927      *
036500990927     C                   IF        T02err = *BLANKS
036600990927     C                   movel     T02ke2        v5cfl
036700990927     c                   else
036800990927     c                   clear                   v5cfl
036900990927     c                   endif
037000990927     c                   iter
037100990927     c                   endif
037200990927     c
037300990927     c     v5cfl         lookup    cfl(i)                                 32
037400990927    4c                   if        not *in32
037500990927     c                   movel     msg(5)        v5msg
037600990927   x4c                   else
037700990927     c                   movel     v5cfl         v4cfl
037800990927     c                   movel     cfld(i)       v4dcfl
037900990927     c
038000990927     c                   seton                                        01
038100990927     c
038200990927    5c                   dow       not *inkl
038300990927     c                   exfmt     fi57w04
038400990927     c                   clear                   v4msg
038500990927     c* f12 - ritorno
038600990927     c   kl              iter
038700990927     c* f16 - annulla cod raggruppamento
038800990927     c                   if        *inkq
038900990927     c                   movel     'A'           watb
039000990927     c                   exsr      scritbe
039100990929     c                   movel     '1'           wesci
039200990929     c
039300990929     c* Tocco come varati tutti i record caricati con qeul raggruppamento
039400990929     c                   z-add     1             nrr1
039500990929     c     nrr1          chain     fi57s03                            32
039600990929     c
039700990929     c                   dow       not *in32
039800990929     c                   if        v3cfl=v4cfl
039900990929     c                   seton                                        27
040000990929     c                   movel(P)  '????????????'v3dcfl
040100990929     c                   update    fi57s03
040200990929     c                   setoff                                       27
040300990929     c                   endif
040400990929     c*
040500990929     c                   add       1             nrr1
040600990929     c     nrr1          chain     fi57s03                            32
040700990929     c                   enddo
040800990929     c                   leave
040900990927     c                   endif
041000990927     c
041100990927    6c                   if        v4dcfl=*blanks
041200990927     c                   movel     msg(3)        v4msg
041300990927    6c                   endif
041400990927     c* f6 - conferma
041500990927    6c                   if        *inkf and v4msg=*blanks
041600990927     c                   movel     ' '           watb
041700990927     c                   exsr      scritbe
041800990927     c                   movel     '1'           wesci
041900990927     c
042000990927     c* Aggiorno tutti i record caricati con quel cod raggruppamento
042100990927     c                   z-add     1             nrr1
042200990927     c     nrr1          chain     fi57s03                            32
042300990927     c
042400990927     c                   dow       not *in32
042500990927     c                   if        v3cfl=v4cfl
042600990927     c                   movel     v4dcfl        v3dcfl
042700990927     c                   update    fi57s03
042800990927     c                   endif
042900990927     c*
043000990927     c                   add       1             nrr1
043100990927     c     nrr1          chain     fi57s03                            32
043200990927     c                   enddo
043300990927     c
043400990927     c                   leave
043500990927    6c                   endif
043600990927    5c                   enddo
043700990927     c
043800990927    4c                   endif
043900990927    3c                   endif
044000990927    2c                   endsl
044100990927     c
044200990927    1c                   enddo
044300990927     c
044400990927     C                   EVAl      *inkf=*off
044500990927     C                   EVAl      *inkl=*off
044600990927     c
044700990927     c                   endsr
044800990927     c*--------------------------------------------------
044900990927     c* Inserimento CODICE RAGGRUPPAMENTO RESPONSABILITA'
045000990927     c*--------------------------------------------------
045100990927     c     insrag        begsr
045200990719     c                   move      *blank        v4cfl
045300990719     c                   move      *blank        v4dcfl
045400990719     c                   move      *blank        v4msg
045500990927     c*
045600990927    1c                   do        *hival
045700990719     c                   exfmt     fi57w04
045800990927     c                   clear                   v4msg
045900990927     c* F12 - ritorno
046000990719     c   kl              leave
046100990719     c
046200990927     c* Controlli
046300990927    2c                   if        v4cfl <> *blank
046400990927     c* non deve essere gia' esistente
046500990927     c     v4cfl         lookup    cfl                                    32
046600990927     c   32              movel     msg(4)        v4msg
046700990927     c   32              iter
046800990927     c
046900990927     c* la decodifica e' obbligatoria
047000990927    3c                   if        v4dcfl=*blanks
047100990927     c                   movel     msg(3)        v4msg
047200990927     c                   iter
047300990927    3c                   endif
047400990927     c
047500990927   x2c                   else
047600990927     c                   movel     msg(4)        v4msg
047700990927     c                   iter
047800990927    2c                   endif
047900990927     c
048000990927     c* f6- conferma
048100990927    2c                   if        *inkf   and v4msg=*blanks
048200990927     c                   movel     ' '           watb
048300990927     c                   exsr      scritbe
048400990927     c                   leave
048500990927    2c                   endif
048600990927     c
048700990927    1c                   enddo
048800990927     c
048900990719     c                   endsr
049000990927     c*--------------------------------------------------
049100990927     c* immetto o aggiorno in tntbe i codici responsabilita'
049200990927     c*--------------------------------------------------
049300990927     c     scritbe       begsr
049400990927     c                   movel(p)  v4cfl         kkey2
049500990927     c     ktbe2         chain     tntbe000                           32
049600990927     c
049700990927     c   32              clear                   tntbe000
049800990927     c                   movel     watb          tbeatb
049900990927     c                   move      'RRD'         tbecod
050000990927     c                   movel(p)  kkey1         tbeke1
050100990927     c                   movel(p)  v4cfl         tbeke2
050200990927     c                   movel(p)  v4dcfl        tbeuni
050300990927     c                   move      'S'           tbeftt
050400990927     c                   move      ' '           tbeftr
050500990927     c   32              write     tntbe000
050600990927     c  n32              update    tntbe000
050700990927     c
050800990927     c                   exsr      carrrd
050900990927     c                   endsr
051000990719      *------------------------------------------------
051100980506     C     SUB_stampa    BEGSR
051200990719      *------------------------------------------------
051300980506      *
051400990722     c                   clear                   s3cfl
051500990722     C                   OPEN      sysprt
051600980508     C                   EXCEPT    TESTA
051700980508      *
051800990706     C     *LOVAL        SETLL     FNDrr000
051900980506      *
052000980508     C                   DO        *HIVAL
052100990706     C                   READ      FNDrr000                               02
052200990722     C   02              leave
052300980508     C   OF              EXCEPT    TESTA
052400990719     c                   z-add     1             i
052500990722     c                   if        drrcfl <> s3cfl
052600990722     C                   except    spazio
052700990722     c                   move      drrcfl        s3cfl
052800990719     c     drrcfl        lookup    cfl(i)                                 66
052900990719     c   66              movel(p)  cfld(i)       s3dcfl
053000990722     c  n66              movel     *blank        s3dcfl
053100990722     c                   setoff                                       65
053200990722     c                   else
053300990722     c                   clear                   s3dcfl
053400990722     c                   seton                                        65
053500990722     c                   end
053600990719     c                   z-add     1             i
053700990719     c     drrtre        lookup    tre(i)                                 66
053800990719     c   66              movel(p)  tred(i)       s3dtre
053900990722     c  n66              movel     *blank        s3dtre
054000990719     c                   select
054100990719     c                   when      drrrta = 'A'
054200990719     c                   move      'AMMANCO '    s3drta
054300990719     c                   when      drrrta = 'M'
054400990719     c                   move      'MANCANZA'    s3drta
054500990719     c                   when      drrrta = 'V'
054600990719     c                   move      'AVARIA  '    s3drta
054700990719     c                   when      drrrta = ' '
054800990719     c                   move      'GENERICO'    s3drta
054900990719     c                   endsl
055000980508     C                   EXCEPT    DETTA
055100990722     C                   ENDDO
055200980508     C                   EXCEPT    CODA
055300990722     C                   CLOSE     sysprt
055400980508      *
055500980506     C                   ENDSR
055600990706     c*--------------------------------------------------
055700990706     c     carrrd        begsr
055800990706     c*--------------------------------------------------
055900990927     c                   clear                   cfl
056000990927     c                   clear                   cfld
056100990927     c
056200990706     C                   movel     'RRD'         codt
056300990706     c                   z-add     1             i                 3 0
056400990706     C     ktbe          setll     tntbe000
056500990706     c                   do        *hival
056600990706     c     ktbe          reade     tntbe000                               99
056700990706     c   99              leave
056800990927     c                   if        tbesif = *blank  and tbeatb=' '
056900990709     c                   movel     tbeke2        cfl(i)
057000990706     c                   movel(p)  tbeuni        cfld(i)
057100990706     c                   add       1             i
057200990706     c                   else
057300990706     c                   iter
057400990706     c                   end
057500990706     c                   enddo
057600990706     c                   endsr
057700990706     c*--------------------------------------------------
057800990706     c     cartre        begsr
057900990706     c*--------------------------------------------------
058000990706     C                   movel     'TRE'         codt
058100990706     c                   z-add     1             i                 3 0
058200990706     C     ktbe          setll     tntbe000
058300990706     c                   do        *hival
058400990706     c     ktbe          reade     tntbe000                               99
058500990706     c   99              leave
058600990924     c                   if        tbesif = *blank  and tbeatb=' '
058700990706     c                   movel(p)  tbeuni        dtreds
058800990706     c                   movel     tbeke1        tre(i)
058900990706     c                   movel(p)  §tredesc      tred(i)
059000990916     c                   move      §tretipo      rta(i)
059100990916     c                   move      '1'           new(i)
059200990706     c                   add       1             i
059300990706     c                   else
059400990706     c                   iter
059500990706     c                   end
059600990706     c                   enddo
059700990706     c                   endsr
059800980506      *****************************************************************
059900980506      *   ROUTINE INIZIALE
060000980506      *****************************************************************
060100980506     C     *INZSR        BEGSR
060200980506      *
060300980506     C     *ENTRY        PLIST
060400980506     C                   PARM                    KPJBA
060500980506      *
060600980506     C                   Z-ADD     1             CODUT
060700980506     C                   CALL      'X§PARUT'
060800980507     C                   PARM                    UT§DSE0F
060900980506     C                   MOVEL     RAGUT         RSUT
061000980508     C                   MOVEL     REC80         CNCR80
061100980506      *
061200980717      * accesso Dettaglio Anagrafica Fasi
061300990706     c     kdrr          klist
061400990706     c                   kfld                    wrfl              2
061500990706     c                   kfld                    wcfl              2
061600990706     c                   kfld                    wrta              1
061700990706     c                   kfld                    wtre              4
061800980717      * accesso tntbe
061900980717     c     KTBE          KLIST
062000980717     C                   KFLD                    CODt
062100990719     c     ktbe2         klist
062200990719     C                   kfld                    kcodt
062300990719     C                   kfld                    kkey1
062400990719     C                   kfld                    kkey2
062500990719     c                   move      'RRD'         kcodt
062600990719     c                   movel(p)  'A1'          kkey1
062700990706      * caricamento schiere da tabelle
062800990706     c                   exsr      carrrd
062900990706     c                   exsr      cartre
063000980717      *
063100980506     C                   ENDSR
063200980506      *****************************************************************
063300990722     Osysprt    E            TESTA             2
063400980507     O                       RSUT              +  0
063500980507     O                                         +  0 '/'
063600980507     O                       SIMFEL            +  0
063700990722     o                                           50 '***  Raggruppamento tipi'
063800990924     o                                         +  3 'responsabilità ***'
063900990722     O                                          115 'FIDN57R'
064000990722     O                                          125 'PAG.'
064100990722     O                       PAGE          Z    132
064200990722     O          E            TESTA             3
064300980507     O                       KNSIF             +  0
064400980507     O                       KNMUS             +  1
064500990722     O          E            TESTA          2  5
064600990722     O                                            4 'Rag'
064700990722     O                                           31 'Cod. Raggruppamento      '
064800990722     O                                           67 'Cod. Tipo Responsabilita'
064900990722     O                                           99 'Tipo Anomalia'
065000980508      *
065100990722     O          E            spazio      1
065200990722      *
065300980508     O          E            DETTA       1
065400990719     o                       drrrfl               3
065500990722     O              n65      Drrcfl            +  3
065600990722     O              n65      s3dcfl            +  2
065700990719     O                       Drrtre            +  3
065800990719     O                       s3dtre            +  2
065900990719     O                       Drrrta            +  3
066000990719     O                       s3drta            +  2
066100990719     O          E            DETTA       0
066200990719     o                       drrrfl               3
066300990719     O          E            DETTA       0
066400990719     o                       drrrfl               3
066500980508      *
066600980508     O          E            CODA        2
066700990722     O                                           64 '***          F I N E'
066800990722     O                                           94 'S T A M P A          ***'
066900980508      *****************************************************************
067000980508** MSG  (Lungh. 78)                                                          *
067100990927Immettere Codice raggruppamento responsabilita' valido                             1
067200980508Eseguita la stampa richiesta                                                       5
067300990927Descrizione obbligatoria
067400990927Immettere cod.raggruppamento NUOVO!!
067500990927Immettere Codice raggr. valido                                                     1
