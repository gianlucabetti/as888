000100991202     H DECEDIT('0,') DATEDIT(*DMY.)
000200991203     H* FNLS85R *----------------------------------------------------*
000300991202     H*            lista mc bollettati per autotrasportatore - filtro
000400991202     H*--------------------------------------------------------------*
000500991202     Ffnapd01l  if   e           k disk
000600991203     Fazorg01l  if   e           k disk
000700991206     Ffnblp34l  if   e           k disk
000800991203     ffnls85d   cf   e             workstn
000900991203     f*
001000991203      * Schiere ------------------------------------------------------------
001100991203     D L1              S              3  0 DIM(30)
001200991203     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001300991203     d*
001400991203      * Definizione campi di work ------------------------------------------
001500991203     d drt_eur         s               D   datfmt(*eur)
001600991203     d drt_iso         s               D   datfmt(*iso)
001700991203     D wtibs69r        s              1
001800991203     D xx              s              3  0
001900991203     d*
002000991203     d TIBS69DS      E DS
002100991203     d DS_cnaco      E DS                  extname(CNACO00F)
002200991203     d DS_cnind      E DS                  extname(CNIND00F)
002300991203     d DS_cnclp      E DS                  extname(CNCLP00F)
002400991203     d DS_fncls      E DS                  extname(FNCLS00F)
002500991203     D* DS PER TRUL06R - CARICAMENTO £X
002600991203     D TRUL06DS      E DS
002700991203     D  LIN                    1     90  0 DIM(30)
002800991203     d*
002900991201     Dkpjba          e ds
003000020430     D*rem                      1      3
003100020430     D*remfil                   4      6  0
003200991203     d                 ds
003300991203     dv01pd1                   1      3  0
003400991203     dv01pd2                   4      7
003500991203     dv01pdr                   1      7
003600991203     d                 ds
003700991203     dv01cm1                   1      3  0
003800991203     dv01cm2                   4      7  0
003900991203     dv01cmm                   1      7  0
004000991203     d*
004100991203     dparam2           ds
004200991203     dparfap                   1      3  0
004300991203     dparpad                   4     10  0
004400991203     dparflg                  11     11
004500991203     dpardes                  12     46
004600991203     d*
004700991203     Dfnls85ds       e ds
004800991203     Dfnls86ds       e ds
004900991202     dcncr80         e ds
005000991202     dut§dse0f       e ds
005100991203     d  tcu                  398    697    DIM(50)
005200991203     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
005300991203     d TCUDS           DS
005400991203     d  f1                     1      1
005500991203     d  f3                     3      3
005600991203     d  f2                     2      2
005700991203     d  f4                     4      4
005800991203     d  f56                    5      6
005900991202     C****************************************************************
006000991202     C*  RIEPILOGO INDICATORI
006100991202     C****************************************************************
006200991202     c* 10    - off=protezione campo ricerca mittente
006300991203     c* 11    - on protezione p.o. autotrasportatore
006400991202     c* 28    - campo messaggio
006500991202     c* 30    - Comodo
006600991202     C* 40/46 - errori video
006700991202     C*****************************************************************
006800991202     c     inizio        tag
006900020430RM*  C*    rem           ifeq      'REM'
007000020430RM*  C*    remfil        andgt     *zeros
007100020430RM*  C*                  z-add     REMFIL        v01pd1
007200020430     c                   z-add     simpou        v01pd1
007300020430     c     simtpp        ifeq      '2'
007400020430     c     simtpp        oreq      *blanks
007500991203     c                   seton                                        11
007600991203RM*  C                   ELSE
007700020430RM*  C*                  z-add     simfel        v01pd1
007800991203     c                   setoff                                       11
007900991203RM*  C                   END
008000991203     c                   movel     '?   '        v01pd2
008100991202     c                   clear                   vd1pdr
008200991202     c                   clear                   v01drt
008300991202     c                   clear                   v01nrt
008400991203     c                   clear                   v01fpp
008500991202     c                   clear                   v01vlr
008600991203     c                   clear                   v01cm1
008700991202     c                   clear                   v01cm2
008800991202     c                   clear                   v01cmr
008900991202     c                   clear                   v01rsm
009000991203     c* se pgm richiamato imposto i campi che ho ricevuto
009100991203     c     d85mod        ifeq      '1'
009200991203     c     d85pdr        ifgt      *zeros
009300991203     c                   move      d85pdr        v01pdr
009400991203     c     d85pdr        chain     fnapd01l                           30
009500991203     c  n30              movel     apdrsc        vd1pdr
009600991203     c                   endif
009700991203     c     d85ksc        ifgt      *zeros
009800991203     c                   z-add     d85ksc        v01cmm
009900991203     C                   clear                   TIBS69DS
010000991203     c                   move      v01cmm        I69kac
010100991203      *
010200991203     C                   CALL      'TIBS69R'
010300991203     C                   PARM                    tibs69DS
010400991203     C                   PARM                    DS_cnaco
010500991203     C                   PARM                    DS_cnind
010600991203     C                   PARM                    DS_cnclp
010700991203     C                   PARM                    DS_fncls
010800991203     c                   movel     acorag        v01cmr
010900991203     c                   endif
011000991207     c                   z-add     WDATE         V01DRT
011100991203     c                   endif
011200991202     c*
011300991202     c     emetti        tag
011400991230     c* imposto indicatore di posizionamento cursore
011500991230     c     v01pd2        ifgt      *zeros
011600991230     c                   seton                                        44
011700991230     c                   else
011800991230     c                   seton                                        41
011900991230     c                   endif
012000991230     c*
012100991203     c                   exfmt     ls85d01
012200991202     c                   clear                   v1cmsg
012300991207     c                   setoff                                       283990
012400991210     c* spengo qui gli indicatori di pos.cursore azichè nella contr
012500991210     c* perchè se uno di questi indicatori è acceso e poi premono f7-ricerca
012600991210     c* mittente il pos.cursore dell'errore vince su quello della ricerca
012700991210     c                   setoff                                       404142
012800991210     c                   setoff                                       444546
012900991202     c     *inkc         cabeq     '1'           fine
013000991202     c     *inkg         ifeq      '1'
013100991203     c                   seton                                        1039
013200991202     c                   goto      emetti
013300991202     c                   endif
013400991202     c                   exsr      contr
013500991202     c  nkf
013600991202     cor 90              goto      emetti
013700991203     c                   clear                   fnls86ds
013800991203     c                   move      v01pdr        d86pdr
013900991206     c                   MOVE      drt_iso       d86drt
014000991206     c                   MOVE      v01drt        d86grt
014100991203     c                   move      v01fpp        d86fpp
014200991203     c                   z-add     v01nrt        d86nrt
014300991203     c                   z-add     v01vlr        d86vlr
014400991203     c                   z-add     v01cmm        d86ksc
014500991203     c     v01cmm        ifgt      *zeros
014600991203     c                   movel     v01cmr        d86rsc
014700991203     c                   else
014800991203     c                   movel     v01rsm        d86rsc
014900991203     c                   endif
015000991203     c                   movel     fnls86ds      kpjbu
015100991203     c                   movel     'LS86'        kcoaz
015200991206     c                   call      'BCH10'
015300991203     c                   parm                    kpjba
015400991201     c*
015500991203     c     fine          tag
015600991206     C                   IF        Wtibs69r  <>  *BLANKS
015700991206     C                   EVAL      I69TLA  = 'C'
015800991206     C                   CALL      'TIBS69R'
015900991206     C                   PARM                    TIBS69DS
016000991206     C                   ENDIF
016100991201     c                   seton                                        lr
016200991202     c**********************************************************************
016300991202     c     contr         begsr
016400991203     c* AUTOTRASPORTATORE
016500991203RM*  C* La filiale padroncino può essere = 0 solo se richiesta la ricer
016600991203RM*  C* ca con ?
016700991203     c     v01pd1        ifeq      *zeros
016800991203     c     '?'           scan      v01pd2                                 31
016900991203     c     *in31         ifeq      *off
017000991203     c                   movel     msg(1)        v1cmsg
017100991203     c                   seton                                        284090
017200991203     c                   goto      endctr
017300991203     c                   endif
017400991203     c                   else
017500991203     c     v01pd1        lookup    l1                                     31
017600991203     c     *in31         ifeq      *off
017700991203     c                   movel     msg(1)        v1cmsg
017800991203     c                   seton                                        284090
017900991203     c                   goto      endctr
018000991203     c                   endif
018100991203     c                   endif
018200991203     c* codice autotrasportatore obbligatorio
018300991203     c     v01pd2        ifeq      *blanks
018400991203     c     v01pd2        oreq      *zeros
018500991203     c                   movel     msg(2)        v1cmsg
018600991203     c                   seton                                        284190
018700991203     c                   goto      endctr
018800991203     c                   endif
018900991203     c* ricerca autotrasportatore
019000991203     c     '?'           scan      v01pd2                                 31
019100991203     c     *in31         ifeq      *on
019200991203     c                   move      *blanks       pardes
019300991203     c                   z-add     0             parpad
019400991203     c                   z-add     v01pd1        parfap
019500991203     c                   movel     'R'           parflg
019600991203     c                   movel     param2        kpjbu
019700991203     c                   call      'FNLV24R'
019800991203     c                   parm                    kpjba
019900991203     c                   movel     kpjbu         param2
020000991203     c                   move      parpad        v01pd2
020100991203     c     parpad        ifne      *zeros
020200991203     c                   movel     parpad        v01pd1
020300991203     c                   endif
020400991203     c                   movel     pardes        vd1pdr
020500991203     c                   seton                                        90
020600991203     c                   goto      endctr
020700991203     c                   endif
020800991206     c* DATA RITIRO MERCE
020900991203     c     v01drt        ifeq      *zeros
021000991203     c                   movel     msg(3)        v1cmsg
021100991203     c                   seton                                        422890
021200991203     c                   goto      endctr
021300991203     c                   endif
021400991203     c     *dmy          test(d)                 v01drt                 30
021500991203     c                   if        *in30 = '0'
021600991203     c     *dmy          move      v01drt        drt_eur
021700991203     c                   else
021800991203     c     *eur          test(d)                 v01drt                 31
021900991203     c                   if        *in31 = '0'
022000991203     c                   move      v01drt        drt_eur
022100991203     c                   else
022200991203     c                   seton                                        284290
022300991203     c                   movel     msg(3)        v1cmsg
022400991203     c                   goto      endctr
022500991203     c                   endif
022600991203     c                   endif
022700991203     c                   move      drt_eur       drt_iso
022800991203     c                   move      drt_eur       v01drt
022900991215     c* METRI CUBI dichiarati obbligatori
023000991203     c     v01vlr        ifeq      *zeros
023100991203     c                   seton                                        442890
023200991203     c                   movel     msg(4)        v1cmsg
023300991203     c                   goto      endctr
023400991203     c                   endif
023500991203     c*
023600991210     c  n10              clear                   v01cmr
023700991206     c* P.O. MITTENTE se immesso deve esistere su organigramma
023800991203     c     v01cm1        ifgt      *zeros
023900991203     c     v01cm1        setll     azorg01l                               30
024000991203     c     *in30         ifeq      *off
024100991203     c                   seton                                        452990
024200991203     c                   movel     msg(5)        v1cmsg
024300991203     c                   goto      endctr
024400991203     c                   endif
024500991203     c                   endif
024600991203     c* non accetto codice 8888 o 9999
024700991203     c     v01cm2        ifeq      8888
024800991203     c     v01cm2        oreq      9999
024900991203     c                   movel     msg(6)        v1cmsg
025000991209     c                   seton                                        462890
025100991203     c                   goto      endctr
025200991203     c                   endif
025300991210     c* se immesso p.o. mittente immettere anche codice mittente
025400991210     c     v01cm1        ifgt      *zeros
025500991210     c     v01cm2        andeq     *zeros
025600991210     c     *in10         andeq     *off
025700991210     c     v01cmr        oreq      *blanks
025800991210     c     v01cm1        andgt     *zeros
025900991210     c     v01cm2        andeq     *zeros
026000991210     c     *in10         andeq     *on
026100991210     c                   movel     msg(10)       v1cmsg
026200991210     c                   seton                                        452890
026300991210     c                   goto      endctr
026400991210     c                   endif
026500991206     c* CODICE MITTENTE deve esistere su p.d.c.
026600991203     c     v01cm2        ifgt      *zeros
026700991203     C                   clear                   TIBS69DS
026800991203     c                   move      v01cmm        I69kac
026900991203      *
027000991203     C                   CALL      'TIBS69R'
027100991203     C                   PARM                    tibs69DS
027200991203     C                   PARM                    DS_cnaco
027300991203     C                   PARM                    DS_cnind
027400991203     C                   PARM                    DS_cnclp
027500991203     C                   PARM                    DS_fncls
027600991203      *
027700991203IF  2C                   eval      *IN28 = (O69ERR =  '1')
027800991203     C                   eval      wtibs69r = '1'
027900991203     c   28              seton                                        4590
028000991203     c   28              movel     msg(7)        v1cmsg
028100991203     c   28              goto      endctr
028200991203     c                   movel     acorag        v01cmr
028300991203     c                   endif
028400991203     c* se richiesta ricerca alfabetica e codice mittente non immesso
028500991203     c* effettuo la ricerca
028600991203     c     *in10         ifeq      *on
028700991203     c     v01cmr        andne     *blanks
028800991203     c     v01cm2        andeq     *zeros
028900991203     c                   movel(p)  v01cmr        pardmt
029000991203     c                   movel     rsut          pardut           30
029100991203     c                   z-add     9             parsta
029200991203     c                   z-add     kci           parkcc
029300991203     c                   movel(p)  v01cm1        parflr
029400991203     c                   clear                   pardit
029500991203     c                   z-add     1             parnum
029600991203     C                   CALL      'XALFA3BR'
029700991203     c* nome azienda
029800991203     C                   PARM                    pardut
029900991203     C                   PARM                    CODUT
030000991203     c* ricerca alfabetica
030100991203     C                   PARM                    pardmt           48
030200991203     c* capoconto
030300991203     C                   PARM                    parkcc            4 0
030400991203     C* esclusione codici bloccati o annullati
030500991203     C                   PARM                    parsta            1 0
030600991203     c* p.o. in cui ricercare
030700991203     C                   PARM                    parflr           90
030800991203     c* azienda in cui ricercare
030900991203     C                   PARM                    pardit            3
031000991203     c                   parm                    parnum            2 0
031100991203     c                   parm                    parkcm           80
031200991203     c                   parm                    parksm          140
031300991203     c                   parm                    parkdm           60
031400991203     c     parsta        ifgt      -1
031500991203     c                   movel     parksm        w0070             7 0
031600991203     c                   move      w0070         v01cm2
031700991203     c                   movel     w0070         v01cm1
031800991203     c                   endif
031900991203     c                   seton                                        90
032000991203     c                   goto      endctr
032100991203     c                   endif
032200991203     c* si può immettere o il codice o la ragione sociale
032300991203     c     v01cmm        ifgt      *zeros
032400991203     c     v01rsm        andne     *blanks
032500991203     c                   seton                                        452890
032600991203     c                   movel     msg(8)        v1cmsg
032700991203     c                   goto      endctr
032800991203     c                   endif
032900991206     c* Controllo se esistono bolle per la ragione sociale immessa
033000991206     c     v01rsm        ifne      *blanks
033100991206     c                   move      v01pdr        d86pdr
033200991206     c                   MOVE      drt_iso       d86drt
033300991206     c     kblp          setll     fnblp34l                               31
033400991206     c     *in31         ifeq      *off
033500991206     c                   seton                                        289040
033600991206     c                   movel     msg(9)        v1cmsg
033700991206     c                   goto      endctr
033800991206     c                   endif
033900991206     c                   endif
034000991203     c
034100991203     c     endctr        tag
034200991203     c                   endsr
034300991201     c**********************************************************************
034400991201     c     *inzsr        begsr
034500991201     c     *entry        plist
034600991201     c                   parm                    kpjba
034700991203     c                   movel     kpjbu         fnls85ds
034800991202     c*
034900991202     C                   Z-ADD     1             codut
035000991202     C                   CALL      'X§PARUT'
035100991202     C                   PARM                    ut§dse0f
035200991202     c                   MOVEL     ragut         rsut             20
035300991202     C                   movel     rec80         cncr80
035400991206     c*
035500991206     c     kblp          klist
035600991206     c                   kfld                    d86pdr
035700991206     c                   kfld                    d86drt
035800991206     c                   kfld                    v01rsm
035900991203     C***
036000991203     C* CARICO TABELLA FILIALI GESTITE £1
036100991203     C***
036200991203     C                   CLEAR                   L1
036300991203     C                   CLEAR                   TRUL06DS
036400991203     C                   MOVE      '£1'          D06COD
036500991203     C                   MOVEL     SIMFEL        D06KEY
036600991203     C                   MOVEL     'L'           D06TLA
036700991203     C                   MOVEL     TRUL06DS      KPJBU
036800991203     C*
036900991203     C                   CALL      'TRUL06R'
037000991203     C                   PARM                    KPJBA
037100991203     C                   MOVEL     KPJBU         TRUL06DS
037200991203     C                   MOVEA     LIN           L1
037300991203      *--- RICERCA CAPOCONTI
037400991203     C                   DO        50            XX
037500991203     C                   MOVE      TCU(XX)       TCUDS
037600991203     C     f56           CABNE     'CG'          END1
037700991203     C     f4            COMP      '1'                                    21
037800991203     C     f4            COMP      '2'                                    22
037900991203     C     f4            COMP      '3'                                    23
038000991203     C     f4            COMP      '6'                                    27
038100991203     C** 1 CLIENTI   21
038200991203     C** 2 FORNITORI 22
038300991203     C** 3 AGENTI    23
038400991203     C     f3            comp      '0'                                242425
038500991203     C     f3            comp      'I'                                    26
038600991203     C** 0 ITALIA   25
038700991203     C** 1 ESTERO   24
038800991203     ** I CAPO CONTO IVA
038900991203     c                   if        *in21 = *on
039000991203     C   24              Z-ADD     KCU(XX)       KCE               4 0
039100991203     C   25              Z-ADD     KCU(XX)       KCI               4 0
039200991203     c                   end
039300991203     c                   if        *in22 = *on
039400991203     C   24              Z-ADD     KCU(XX)       KFE               4 0
039500991203     C   25              Z-ADD     KCU(XX)       KFI               4 0
039600991203     c                   end
039700991203     c                   if        *in23 = *on
039800991203     C   24              Z-ADD     KCU(XX)       KAE               4 0
039900991203     C   25              Z-ADD     KCU(XX)       KAI               4 0
040000991203     c                   end
040100991203     C   26              Z-ADD     KCU(XX)       KIVA              4 0
040200991203     C   27              Z-ADD     KCU(XX)       KBNA              4 0
040300991203     C     end1          tag
040400991203     C                   end
040500991203     C                   SETOFf                                       212223
040600991203     C                   sETOFf                                       242526
040700991203     C                   SETOFf                                       27
040800991207     c*
040900991207     C                   time                    wtime            14 0
041000991207     C                   move      wtime         wdate             8 0
041100991203     c*
041200991201     c                   endsr
041300991203**
041400991203Immettere p.o.autotrasportatore valido o '?' nel relativo codice per l'elenco 01
041500991203Immettere un codice autotrasportatore valido                                  02
041600991203Immettere una data valida                                                     03
041700991215Metri Cubi Dichiarati obbligatori                                             04
041800991203P.o. Mittente errato                                                          05
041900991203Codice non ammesso                                                            06
042000991203Cod mittente inesistente su Piano dei Conti                                   07
042100991203Immettere il Codice Mittente  OPPURE la ragione sociale                       08
042200991206Non trovati dati da elaborare per la selezione immessa                        09
042300991210Se immesso p.o. mittente immettere anche il codice
