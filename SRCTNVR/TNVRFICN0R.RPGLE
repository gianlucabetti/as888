000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400020403     H/TITLE  contabilizzazione conteggi
000500951002?     *--------------------------------------------------------------*
000600020403      * FICN50R                                                      *
000700951002      *     *----------------------------------------------*         *
000800020403      *         CONTABILIZZAZIONE CONTEGGI PARAMETRI                 *
000900951002      *     *---------------------------------------------*          *
001000951002      *                                                              *
001100951002?     *--------------------------------------------------------------*
001200951002      * KC - FINE LAVORO                                             *
001300951002?     *--------------------------------------------------------------*
001400951002      * 28 - Visualizzazione messaggio di errore                     *
001500951002      * 30 - Indicatore per chain generiche                          *
001600951017      * 31 - Indicatore per LOKUP                                    *
001700020403      * 40 - Reverse Immage su codice autotrasp. 1                   *
001800020403      * 42 - Reverse Immage su codice autotrasp. 2                   *
001900020403      * 41 - Reverse Immage su data dal                              *
002000020410      * 45 - Reverse Immage su data al                               *
002100951002      * 43 - Reverse Immage su numero fattura                        *
002200951002      * 44 - Reverse Immage su data fattura                          *
002300020403      * 48 - Reverse Immage su p.o.                                  *
002400020403      * 50 - Reverse Immage su descrizione autotrasp. 1              *
002500020403      * 52 - Reverse Immage su descrizione autotrasp. 2              *
002600020403      * 98 - Richiamo pgm ricerca codice autotrasportatore           *
002700951002?     *--------------------------------------------------------------*
002800020417     Fanuni01l  IF   E           K DISK
002900030204     Fancdp01l  IF   E           K DISK
003000020417     Fazorg01l  IF   E           K DISK
003100020417     Fansog01l  IF   E           K DISK
003200020417     Fanfrn01l  IF   E           K DISK
003300020417     Fanrco01l  IF   E           K DISK
003400020417     Ftntbe01l  IF   E           K DISK
003500020510     Ftntlz01l  IF   E           K DISK
003600020417     Fficn50D   CF   E             WORKSTN
003700951002     D*---------------------------------------------------------------*
003800951002     D* SCHIERA
003900951002     D*---------------------------------------------------------------*
004000030827     D ERR             S             70    DIM(24) CTDATA PERRCD(1)             Errori
004100980910     D* Definizioni parametri chiamata moduli NDMVC104
004200980910    >D Societa         S              3
004300980910    >D Ctb             S              2
004400980910    >D Unita           S              4
004500980910     D TpReg           S              1
004600980911     D DtReg           S             10
004700980910     D DtLGio          S             10
004800980910     D RegIVA          S              1
004900980910     D TpRegIVA        S              1
005000980910     D Stato           S              2
005100980910     D Esito           S              1
005200980910     D Gesmsg          S              1
005300980910     D* Definizione DS per recupero msg generati dai moduli
005400980910     D QLenMsg         S              9B 0
005500980910     D QFormat         S              8
005600980910     D QCallMsg        S             10
005700980910     D QCallStack      S              9B 0
005800980910     D QMsgTyp         S             10
005900980910     D QMsgKey         S              4
006000980910     D QWait           S              9B 0
006100980910     D QAction         S             10
006200980910     D QErrCod         DS
006300980910     D  Q1ErrLen                      9B 0 INZ(16)
006400980910     D  Q1LenRet                      9B 0 INZ(16)
006500980910     D  Q1ErrId                       7
006600980910     D*-------
006700980910     D QInfo           DS
006800980910     D  Q2BytRet                      9B 0
006900980910     D  Q2BytAva                      9B 0
007000980910     D  Q2Severity                    9B 0
007100980910     D  Q2MsgId                       7
007200980910     D  Q2MsgTyp                      2
007300980910     D  Q2MsgKey                      4
007400980910     D  Q2Reserve                    15
007500980910     D  Q2MsgLen                      9B 0
007600980910     D  Q2MsgLenAv                    9B 0
007700980910     D  Q2MsgTxt                     80
007800980910?     *--------------------------------------------------------------*
007900980910?     *  DS                                                          *
008000980910?     *--------------------------------------------------------------*
008100980910     D DSFMT           DS
008200980910     D  $TASTO               369    369
008300980910     D  NRG                  370    370
008400980910     D  NCL                  371    371
008500980910     D*-------------
008600980910     D* posizione cursore
008700980910     D CURSOR          DS
008800980910     D  RIRI                   1      2B 0 INZ
008900980910     D  R$$                    2      2
009000980910     D  COCO                   3      4B 0 INZ
009100980910     D  C$$                    4      4
009200980910     D*-------------
009300980910     D* Reperimento nome PGM
009400020510     D                SDS
009500020510     D  nompgm           *PROC
009600951002     D KPJBA         E DS
009700020722     D* LIBAZP               247    256
009800020722     D* LIBGES               257    266
009900020404     Ddtfmese                          d   datfmt(*iso)
010000020404     Ddataiso                          d   datfmt(*iso)
010100020415     Doggiiso                          d   datfmt(*iso)
010200980910     Ddataeur                          d   datfmt(*eur)
010300020417     Dkksc             s                   like(v1cpdr)
010400980911     D WData0          C                   CONST('0001-01-01')
010500020403     D ficn50ds      E DS
010600020404     D A009DS        E DS                  EXTNAME(ANA009DS) INZ
010700020423     D XAUTUNIDS     e ds                  inz
010800020415     D dblc          E DS
010900030128     D dblo          E DS
011000030827     D dbld          E DS
011100020513     D dtlz01        E DS
011200030206     D ANA024DS      E DS
011201031121     c* parametri entrata
011202031121     D dspar           DS
011203031121     D  tipo                          1
011204031121     D  causale                       4
011300960215     D*------------------
011400020403     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
011500960215     D*------------------
011600960215     D PARAM           DS                  INZ
011700020415     D* TIPOLOGIA P_=autotrasport.
011800000811     D  PARTIP                 1      2
011900020415     D* RAGIONE SOCIALE aoutotras.
012000000811     D  PARRSC                 3     37
012100020415     D* S.I.
012200000811     D  PARCSF                38     40
012300020415     D* CODICE
012400000811     D  PARPDR                41     47  0
012500020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
012600000811     D*                  ALFABETICA RICHIESTA
012700000811     D  PARFLG                48     48
012800980910     D soc001        E DS                  EXTNAME(xsoc001ds)
012900980910     D xsocds          DS          1000
013000990506     D*-------------
013100020403     D com13           s             13    inz('0000000000000')
013200990506     D*  Prototipazione delle procedure
013300990506     D/COPY *LIBL/SRCCPY,PJX002PR
013400951002?     *--------------------------------------------------------------*
013500951002?     *  CICLO PRINCIPALE                                            *
013600951002?     *--------------------------------------------------------------*
013700951002      *  Impostazione parametri prima videata
013800951002     C                   EXSR      INZ01
013900951002      *  Loop gestione videata
014000951002     C     WFINE         DOUEQ     'S'
014100951002     C                   EXSR      GESD01
014200951002     C                   END
014300951002      *  Fine Lavoro
014400951002     C     FINE          TAG
014500951002     C                   SETON                                        LR
014600951002?     *--------------------------------------------------------------*
014700951002?     *  INZ01: Inizializzazione prima videata                       *
014800951002?     *--------------------------------------------------------------*
014900951002     C     INZ01         BEGSR
015000951002      *
015100020403     C                   SETOFF                                       2898
015200020403     C                   movea     com13         *in(40)
015300020411     C                   movel     *blanks       V1CPDR
015400020411     C                   movel     *blanks       V2CPDR
015500951002     C                   Z-ADD     0             V1CDFT
015600951002     C                   Z-ADD     0             V1CNFT
015700030620     c* per le cooperative devo fatturare solo il mese
015800030620     c                   if        tipo = 'A'
015900020509     C                   Z-ADD     0             V1Cdti
016000030620     c                   else
016100030620     C                   Z-ADD     v1cdpr        V1Cdti
016200030620     C                   movel     01            V1Cdti
016300030620     c                   end
016400020509     C                   Z-ADD     v1cdpr        V1Cdtf
016500020404     C                   MOVEL     *BLANKS       V1DPDR
016600020403     C                   MOVEL     *BLANKS       V2DPDR
016700030128     C                   MOVEL     'S'           tsrv
016800030204     C                   MOVEL     *BLANKS       condpv
016900030204     C                   MOVEL     *BLANKS       descon
017000951002     C*
017100951002     C                   ENDSR
017200951002?     *--------------------------------------------------------------*
017300951002?     *  GESD01: Gestione prima videata                              *
017400951002?     *--------------------------------------------------------------*
017500951002     C     GESD01        BEGSR
017600951002      *
017700980910     C                   EXFMT     y350d01
017800020403     C                   SETOFF                                       2898
017900020403     C                   movea     com13         *in(40)
018000020306     c                   movea     '00000000'    *in(71)
018100951002      *  Fine Lavoro
018200951002     C     *INKC         IFEQ      '1'
018300951002     C                   MOVEL     'S'           WFINE
018400951002     C                   GOTO      FINVD1
018500951002     C                   END
018600020403     C*  effettua ricerche per fornitore
018700980910     c                   if        *inkd = *on
018800980910     c                   exsr      search
018900020503     C                   GOTO      FINVD1
019000980910     c                   end
019100951002      *  Controlli
019200951002     C                   EXSR      CTR01
019300951002     C   28              GOTO      FINVD1
019400951002      *  No errori: call TNTL41R
019500020404     C                   EXSR      CALL51
019600951002      *
019700951002     C     FINVD1        ENDSR
019800951002?     *--------------------------------------------------------------*
019900951002?     *  CTR01: Controlli prima videata                              *
020000951002?     *--------------------------------------------------------------*
020100951002     C     CTR01         BEGSR
020200020423     C* Verifico unità di registrazione
020300020423     c                   movel     *blanks       v1cdun
020400020423     c                   if        v1cuni = *blanks
020500020423     C                   SETON                                        4628
020600020423     C                   MOVEL     ERR(15)       $MSG
020700020423     C                   GOTO      FINCT1
020800020423     c                   else
020900020423     c     kuni          chain     anuni01l                           46
021000020423     c                   if        not *in46
021100020423     c                   movel     unidesbrev    v1cdun
021200020423     C* CONTROLLO SE L'UTENTE E' ABILITATO AD USARE IL CDC
021300020423     C                   CLEAR                   XAUTUNIDS
021400020423     C                   MOVEL     KNMUS         XAUPRF
021500020423     C                   MOVEL     XSCSOC        XAUSOC
021600020423     C                   MOVEL     V1cuni        XAUUNI
021700020423     C                   CALL      'XAUTUNI'
021800020423     C                   PARM                    XAUTUNIDS
021900020423     C                   IF        XAUABL = '0'
022000020423     C                   SETON                                        4628
022100020423     C                   MOVEL     ERR(17)       $MSG
022200020423     C                   GOTO      FINCT1
022300020423     C                   END
022400020423     c                   else
022500020423     C                   SETON                                        28
022600020423     C                   MOVEL     ERR(15)       $MSG
022700020423     C                   GOTO      FINCT1
022800020423     c                   end
022900020423     c                   end
023000020724     C* Verifico esistenza tabella YIV
023100020724     C                   eval      tbecod = 'YIV'
023200020724     C                   eval      tbeke1 = xscsoc
023300020724     C                   eval      tbeke2 = v1cuni
023400020724     C     ktbe1         chain     tntbe01l
023500020724     C                   if        not %found(tntbe01l)
023600020423     C                   SETON                                        4628
023700020423     C                   MOVEL     ERR(14)       $MSG
023800020423     C                   GOTO      FINCT1
023900020423     C                   endif
024000020408     C* CONTROLLO se posso lanciare la contabilizzazione
024100030827     c                   select
024200030827     c                   when      tipo = 'A'
024300020513     c                   move      §blcdbl       dataiso
024400030827     c                   when      tipo = 'C'
024500030128     c                   move      §blodbl       dataiso
024600030827     c                   when      tipo = 'D'
024700030827     c                   move      §blddbl       dataiso
024800030827     c                   endsl
024900020415     c                   move      *date         dataeur
025000020415     c                   move      dataeur       oggiiso
025100020415     c                   if        oggiiso > dataiso
025200020408     c                   seton                                        28
025300030827     c                   select
025400030827     c                   when      tipo = 'A'
025500020408     C                   MOVEL     ERR(13)       $MSG
025600030827     c                   when      tipo = 'C'
025700030128     C                   MOVEL     ERR(22)       $MSG
025800030827     c                   when      tipo = 'D'
025900030827     C                   MOVEL     ERR(24)       $MSG
026000030827     c                   endsl
026100020408     C                   GOTO      FINCT1
026200020408     c                   end
026300020404      *  Controllo codice fornitore   dal
026400020503     C                   clear                   v1dpdr
026500020423     C     V1CPDR        IFne      *blanks
026600020411     C                   move      V1CPDR        kksc
026700980910     c                   movel     *blank        sogdes
026800980910     C     Kfrn          CHAIN     anfrn01l                           30
026900980910     C  N30Krco          CHAIN     anrco01l                           30
027000980910     C  N30rcosogg       CHAIN     ansog01l                           30
027100020403     C                   MOVEL     sogdes        f50DP1
027200980911     C                   MOVEL(p)  sogdes        v1dpdr
027300951002     C     *IN30         IFEQ      '1'
027400951002     C                   SETON                                        4028
027500951002     C                   MOVEL     ERR(2)        $MSG
027600951009     C                   GOTO      FINCT1
027700951002     C                   END
027800030210     C                   if        TIPO = 'A' and
027900030210     C                             rcoctgan02 <> 'N   ' and
028000020527     C                             rcoctgan02 <> 'I   ' and
028100020527     C                             rcoctgan02 <> 'M   '
028200980910     C                   SETON                                        4028
028300980910     C                   MOVEL     ERR(3)        $MSG
028400980910     C                   GOTO      FINCT1
028500980910     c                   end
028600020423     C                   if        rcounita <> v1cuni
028700020423     C                   SETON                                        4028
028800020423     C                   MOVEL     ERR(16)       $MSG
028900020423     C                   GOTO      FINCT1
029000020423     c                   end
029100020423     c                   end
029200020404      *  Controllo codice fornitore   al
029300020503     C                   clear                   v2dpdr
029400020411     C     V2CPDR        IFEQ      *blanks
029500020411     c                   movel     v1cpdr        v2cpdr
029600020411     C                   END
029700020423     C     V2CPDR        IFne      *blanks
029800020411     C                   move      V2CPDR        kksc
029900020403     c                   movel     *blank        sogdes
030000020403     C     Kfrn          CHAIN     anfrn01l                           30
030100020403     C  N30Krco          CHAIN     anrco01l                           30
030200020403     C  N30rcosogg       CHAIN     ansog01l                           30
030300020403     C                   MOVEL     sogdes        f50DP2
030400020403     C                   MOVEL(p)  sogdes        v2dpdr
030500020403     C     *IN30         IFEQ      '1'
030600020410     C                   SETON                                        4228
030700020403     C                   MOVEL     ERR(2)        $MSG
030800020403     C                   GOTO      FINCT1
030900020403     C                   END
031000030210     C                   if        tipo = 'A' and
031100030210     C                             rcoctgan02 <> 'N   ' and
031200020527     C                             rcoctgan02 <> 'I   ' and
031300020527     C                             rcoctgan02 <> 'M   '
031400020410     C                   SETON                                        4228
031500020403     C                   MOVEL     ERR(3)        $MSG
031600020403     C                   GOTO      FINCT1
031700020403     c                   end
031800020423     C                   if        rcounita <> v1cuni
031900020423     C                   SETON                                        4228
032000020423     C                   MOVEL     ERR(16)       $MSG
032100020423     C                   GOTO      FINCT1
032200020423     c                   end
032300020423     c                   end
032400020411     C*
032500020411     c                   if        v1cpdr > v2cpdr
032600020503     C                   MOVEL     ERR(18)       $MSG
032700020411     C                   SETON                                        404228
032800020411     C                   GOTO      FINCT1
032900020411     c                   end
033000020403     c* data da
033100020404     c                   if        v1cdti <> 0
033200980911     c     *eur          test(d)                 v1cdti                 41
033300980911     c                   if        *in41
033400980911     c     *dmy          test(d)                 v1cdti                 41
033500980911     c  n41*dmy          move      v1cdti        dataeur
033600980911     c  n41              move      dataeur       v1cdti
033700980911     c                   end
033800020404     c                   end
033900020403     c* data al
034000020404     c                   if        v1cdtf <> 0
034100020410     c     *eur          test(d)                 v1cdtf                 45
034200020415     c                   if        *in45
034300020410     c     *dmy          test(d)                 v1cdtf                 45
034400020415     c  n45*dmy          move      v1cdtf        dataeur
034500020415     c  n45              move      dataeur       v1cdtf
034600980911     c                   end
034700020404     c                   else
034800020410     c                   seton                                          2845
034900020404     c                   movel     err(4)        $msg
035000020404     c                   goto      finct1
035100020404     c                   end
035200020415     c                   if        *in41 or *in45
035300980910     c                   seton                                          28
035400980910     c                   movel     err(5)        $msg
035500980910     c                   goto      finct1
035600020404     c                   else
035700020404     c                   if        v1cdti <> 0
035800980910     c                   movel     v1cdti        dataeur
035900980910     c                   movel     dataeur       dataiso
036000980910     c                   movel     dataiso       wdatda            8 0
036100020404     c                   else
036200020404     c                   z-add     0             wdatda
036300020404     c                   end
036400980910     c                   movel     v1cdtf        dataeur
036500980910     c                   movel     dataeur       dataiso
036600980910     c                   movel     dataiso       wdataa            8 0
036700951002      *  Controllo limiti due date
036800951002     C     WDATDA        IFGT      WDATAA
036900020410     C                   SETON                                        414528
037000951002     C                   MOVEL     ERR(6)        $MSG                           Data iniz.>fina.
037100951002     C                   GOTO      FINCT1
037200951002     C                   END
037300980910     c                   end
037400951002      *  Controllo data Fattura
037500020403     C     V1CDFT        IFne      0
037600980911     c     *eur          test(d)                 v1cdft                 44
037700980911     c                   if        *in44
037800980911     c     *dmy          test(d)                 v1cdft                 44
037900980911     c  n44*dmy          move      v1cdft        dataeur
038000980911     c  n44              move      dataeur       v1cdft
038100980911     c                   end
038200980910     c                   if        *in44
038300980910     C                   SETON                                          28
038400951002     C                   MOVEL     ERR(5)        $MSG                           Data errata
038500951002     C                   GOTO      FINCT1
038600980910     c                   else
038700980910     c                   movel     v1cdft        dataeur
038800980910     c                   movel     dataeur       dataiso
038900980910     c                   movel     dataiso       wdatfa            8 0
039000020404     c                   end
039100020403     c                   if        v1cnft = 0
039200020403     C                   SETON                                        4328
039300020404     C                   MOVEL     ERR(7)        $MSG                           Data prot.<dat.fatt.
039400020403     C                   GOTO      FINCT1
039500020403     c                   end
039600020403     c                   else
039700020403     c                   if        v1cnft <> 0
039800020404     C                   SETON                                        4428
039900020404     C                   MOVEL     ERR(5)        $MSG                           Data prot.<dat.fatt.
040000020403     C                   GOTO      FINCT1
040100020403     c                   end
040200020404     c                   z-add     0             wdatfa
040300020403     c                   end
040400020411     c*
040500020509     c                   if        v1cnft <> 0
040600020509     c                   if        v1cpdr <> v2cpdr
040700020509     c                             or v1cpdr = *blanks
040800020509     C                   SETON                                        442843
040900020411     C                   MOVEL     ERR(11)       $MSG                           Data prot.<dat.fatt.
041000020411     C                   GOTO      FINCT1
041100020411     c                   end
041200020509     c* controllo se esiste un fornitore di fatturazione dal quale
041300020509     c* la numerazione fattura
041400020509     c                   if        frnforfatt <> *blanks
041500020509     C                   SETON                                        442843
041600020509     C                   MOVEL     ERR(19)       $MSG                           Data prot.<dat.fatt.
041700020509     C                   GOTO      FINCT1
041800020509     c                   end
041900030213     c                   end
042000020515     c* controllo se il fornitore è abilitato all'uso della propria
042100020515     c* numerazione
042200020515     c                   move      v1cpdr        tlzpdr
042300030827     c                   select
042400030827     c                   when      tipo = 'A'
042500030211     c                   movel(p)  'P'           tlztip
042600030827     c                   when      tipo = 'C'
042700030211     c                   movel(p)  'C'           tlztip
042800030827     c                   when      tipo = 'D'
042900030827     c                   movel(p)  'D'           tlztip
043000030827     c                   endsl
043100020515     c     ktlz          chain     tntlz01l
043200030213     c                   if        %found(tntlz01l)
043300020515     c                   movel     tlzflo        dtlz01
043400030213     c                   if        §tlzfl1 = ' '
043500020515     c                   if        v1cnft <> 0
043600020515     C                   SETON                                        442843
043700020515     C                   MOVEL     ERR(20)       $MSG                           Data prot.<dat.fatt.
043800020515     C                   GOTO      FINCT1
043900020515     c                   end
044000030213     c                   else
044100020515     c                   if        v1cnft = 0
044200020515     C                   SETON                                        442843
044300020515     C                   MOVEL     ERR(21)       $MSG                           Data prot.<dat.fatt.
044400020515     C                   GOTO      FINCT1
044500020515     c                   end
044600030213     c                   end
044700030213     c                   if        tlzatb <> ' '
044800030211     C                   SETON                                        4228
044900030211     C                   MOVEL     ERR(2)        $MSG
045000030211     C                   GOTO      FINCT1
045100030213     c                   end
045200030213     c                   else
045300030213     c                   if        tipo = 'A' and v1cpdr <> *blanks
045400030211     C                   SETON                                        4228
045500030211     C                   MOVEL     ERR(2)        $MSG
045600030211     C                   GOTO      FINCT1
045700030211     c                   end
045800030211     c                   end
045900020404      *  Data Protocollo < data bollato
046000980910     c                   exsr      mvc104
046100980910     c                   if        esito = *on
046200980910     C                   GOTO      FINCT1
046300980910     c                   end
046400951011      *  Data Protocollo < data fattura
046500951011     C     WDATPR        IFLT      WDATFA
046600951011     C                   SETON                                        4828
046700020404     C                   MOVEL     ERR(9)        $MSG                           Data prot.<dat.fatt.
046800951011     C                   GOTO      FINCT1
046900951011     C                   END
047000020408      *  Data Protocollo < data finale
047100020408     C     WDATPR        IFLT      WDATaa
047200020410     C                   SETON                                        4528
047300020408     C                   MOVEL     ERR(12)       $MSG                           Data prot.<dat.fatt.
047400020408     C                   GOTO      FINCT1
047500020408     C                   END
047600030204      *  Condizioni di pagamento solo per Coop
047700030204     C                   IF        tipo = 'C'
047800030204     c                   CLEAR                   DESCON
047900030204     C                   IF        condpv <> *blanks
048000030204     c     kcdp          chain     ancdp01l
048100030204     c                   if        %found(ancdp01l)
048200030204     c                   movel     cdpdesbrev    descon
048300030204     C                   ELSE
048400030204     C                   SETON                                        4928
048500030204     C                   MOVEL     ERR(23)       $MSG                           Data prot.<dat.fatt.
048600030204     C                   GOTO      FINCT1
048700030204     C                   END
048800030204     C                   END
048900030204     C                   END
049000951003      *
049100951009     C     FINCT1        ENDSR
049200980910     C************************************************************
049300980910     C* Chiamata al modulo di controllo data registrazione
049400980910     C************************************************************
049500980910     C     MVC104        BEGSR
049600980910     C*
049700980910     C                   move      '046'         Unita
049800980911     C                   move      wdatpr        Dataiso
049900980911     C                   move      dataiso       DtReg
050000980911     C                   move      wdata0        Dtlgio
050100980910     C                   CALLB     'NDMVC104'
050200980910     C                   PARM      xscsoc        Societa
050300980910     C                   PARM      'CG'          CTB
050400980910     C                   PARM                    Unita
050500980910     C                   PARM      'D'           TpReg
050600980910     C                   PARM                    DTReg
050700980910     C                   PARM      '1'           RegIVA
050800980910     C                   PARM      '1'           TpRegIVA
050900980910     C                   PARM      '01'          Stato
051000980910     C                   PARM      *on           GesMsg
051100980910     C                   PARM                    Esito
051200980910     C*
051300980910     C* se ci sono errori
051400980910     C                   IF        Esito = *on
051500980910     C                   seton                                        48
051600980910     C* gestisco la loro emissione
051700980910     C                   DOU       Q2BytAva = 0
051800980910     C                   EXSR      RestoreMsg
051900980910     C                   IF        Q2BytAva > 0
052000980910     C                   EXSR      GestErr
052100980910     C                   ENDIF
052200980910     C                   ENDDO
052300980910     C*
052400980910     c                   else
052500980910     C                   CALLB     'NDMVC106'
052600980910    >C                   PARM                    Societa
052700980910    >C                   PARM                    Ctb
052800980910    >C                   PARM                    TpReg
052900980910    >C                   PARM                    DtLGio
053000980910    >C                   PARM                    DtReg
053100980910     C                   PARM                    GesMsg
053200980910     C                   PARM                    Esito
053300980910     c                   if        esito = *on
053400980910     C                   SETON                                        4828
053500020404     C                   MOVEL     ERR(8)        $MSG                           Data prot.<=bollato
053600980910     C                   END
053700980910     C                   ENDIF
053800980910     C*
053900980910     C*
054000980910     C                   ENDSR
054100980910     C************************************************************
054200980910     C* Recupero i msg sono stati generati dal modulo chiamato
054300980910     C************************************************************
054400980910     C     RestoreMsg    BEGSR
054500980910     C*
054600980910     C                   CALL      'QMHRCVPM'
054700980910     C                   PARM                    QInfo
054800980910     C                   PARM      113           QLenMsg
054900980910     C                   PARM      'RCVM0100'    QFormat
055000980910     C                   PARM      '*'           QCallMsg
055100980910     C                   PARM      0             QCallStack
055200980910     C                   PARM      '*DIAG'       QMsgTyp
055300980910     C                   PARM                    QMsgKey
055400980910     C                   PARM      0             QWait
055500980910     C                   PARM      '*REMOVE'     QAction
055600980910     C                   PARM                    QErrCod
055700980910     C*
055800980910     C                   ENDSR
055900980910     C************************************************************
056000980910     C* Gestione messaggi per il campo causale di testata
056100980910     C************************************************************
056200980910     C     GestErr       BEGSR
056300980910     C*
056400980910     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
056500980910     C                   MOVE      Q2MsgId       IdMsg
056600980910     C                   CALLB     'XRTVM'
056700980910     C                   PARM                    IDMSG            27
056800980910     C                   PARM                    TXTMSG          100
056900980910     C                   PARM                    ERRMSG            1
057000980910     C                   PARM                    *omit
057100980910     C                   PARM                    *omit
057200980910     C                   PARM      Q2msgtxt      Msgdta          100
057300980910     C                   IF        ErrMsg <> *on
057400020417     C                   eval      $msg = txtmsg
057500980910     C                   SETON                                        2848
057600980910     C                   ENDIF
057700980910     C*
057800980910     C                   ENDSR
057900020404     C************************************************************
058000020404     C* RICERCHE
058100020404     C************************************************************
058200020404     C     SEARCH        BEGSR
058300980910     C*
058400980910     C* determino Riga/Colonna del cursore
058500980910     C                   MOVE      nrg           R$$
058600980910     C                   MOVE      ncl           C$$
058700980910     C                   Z-ADD     RIRI          CURR
058800980910     C                   Z-ADD     COCO          CURC
058900980910     C*
059000980910     C* I campi che al momento dell'F4 risultano protetti non devono
059100980910     C* essere abilitati alla funzione di ricerca
059200980910     C*
0593009809101    C                   SELECT
059400020423     C* FMTD1  - ricerca unità di reg.
059500020423    >C     H1NMFL        WHEneq    'V1CUNI'
059600020423     C                   RESET                   A009DS
059700020423     C                   MOVE      XSCSOC        SOC009
059800020423     C                   EVAL      OPZ009 = '01'
059900020423     C                   EVAL      Kpjbu = A009DS
060000020423     C                   CALL      'ANA009R'
060100020423     C                   PARM                    KPJBA
060200020423     C                   EVAL      A009DS = Kpjbu
060300020423      *
060400020423     C     UNI009        IFNE      *BLANK
060500020423     C                   MOVE      UNI009        v1cuni
060600020423     C                   ENDIF
060700030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
060800030422    >C                   WHEn      H1NMFL = 'V1CPDR' AND
060900030422     c                             tipo = 'A'
061000980910     C*  deposita la KPJBU
061100980910     C                   MOVEL     KPJBU         DKPJBU          256
061200020503     C                   movel     xscsoc        parcsf
061300020404     C                   MOVEl     V1DPDR        PARRSC
061400980910     C                   Z-ADD     *ZEROS        PARPDR
061500020403     C                   eval      partip = 'P '
061600980910     C                   MOVEL     *BLANKS       PARFLG
061700020419     C                   MOVEL(p)  PARAM         KPJBU
061800980910     C                   CALL      'TRUL16R'
061900980910     C                   PARM                    KPJBA
062000980910     C                   MOVEL     KPJBU         PARAM
062100980910IF  2C     PARFLG        IFEQ      '3'
062200980910     C                   MOVEL     *BLANKS       V1DPDR
062300020403     C                   SETON                                        5028
062400020404     C                   MOVEL     ERR(10)       $MSG
062500980910X   2C                   ELSE
062600020422     C                   MOVEl     *zeros        V1CPDR
062700020422     C                   MOVE      PARPDR        V1CPDR
062800020403     C                   MOVEL     PARRSC        V1DPDR
062900980910E   2C                   ENDIF
063000030422     C* FMTD1  - ricerca fornitore 2 AUTOTRASPORTATRE
063100030422    >C                   WHEn      H1NMFL = 'V2CPDR' AND
063200030422     c                             tipo = 'A'
063300020403     C*  deposita la KPJBU
063400020403     C                   MOVEL     KPJBU         DKPJBU          256
063500020404     C                   MOVEl     V2DPDR        PARRSC
063600020403     C                   Z-ADD     *ZEROS        PARPDR
063700020403     C                   eval      partip = 'P '
063800020503     C                   movel     xscsoc        parcsf
063900020403     C                   MOVEL     *BLANKS       PARFLG
064000020419     C                   MOVEL(P)  PARAM         KPJBU
064100020403     C                   CALL      'TRUL16R'
064200020403     C                   PARM                    KPJBA
064300020403     C                   MOVEL     KPJBU         PARAM
064400020403IF  2C     PARFLG        IFEQ      '3'
064500020403     C                   MOVEL     *BLANKS       V2DPDR
064600020403     C                   SETON                                        5228
064700020404     C                   MOVEL     ERR(10)       $MSG
064800020403X   2C                   ELSE
064900020422     C                   MOVEl     *zeros        V2CPDR
065000020404     C                   MOVE      PARPDR        V2CPDR
065100020403     C                   MOVEL     PARRSC        V2DPDR
065200020403E   2C                   ENDIF
065300030206     C* FMTD1  - ricerca condizioni pagamento
065400030206    >C     H1NMFL        WHEneq    'CONDPV'
065500030206     C*  deposita la KPJBU
065600030206     C                   MOVEL     KPJBU         DKPJBU          256
065700030206     C                   RESET                   ANA024DS
065800030206     C                   MOVE      '01'          OPZ024
065900030206     C                   MOVEL(p)  ANA024DS      KPJBU
066000030206     C                   CALL      'ANA024R'
066100030206     C                   PARM                    KPJBA
066200030206     C                   MOVEL     KPJBU         ANA024DS
066300030206     C     Cod024        IFNE      *BLANK
066400030206     C                   MOVEL     Cod024        condpv
066500030206     c                   else
066600030206     c                   movel     *blanks       condpv
066700030206     C                   ENDIF
066800020510     C                   ENDsl
066900980910     C* REIMPOSTA LA KPJBU
067000980910     C                   MOVEL     DKPJBU        KPJBU
067100980910     C                   SETON                                        98
067200980910     C* imposto pos. del cursore
067300980910     C                   Z-ADD     CURR          H1RIGA
067400980910     C                   Z-ADD     CURC          H1COLO
067500980910     C                   ENDsr
067600951006?     *--------------------------------------------------------------*
067700020404?     *  CALL51: Controlli prima videata                             *
067800951006?     *--------------------------------------------------------------*
067900020404     C     CALL51        BEGSR
068000951006      *
068100020404     c                   clear                   ficn50ds
068200030128     C                   movel     tipo          f50tip
068300030204     C                   movel     condpv        f50cdp
068400030128     C                   movel     tsrv          f50tsr
068500030128     C                   movel     xscsoc        f50soc
068600020423     c                   movel     v1cuni        f50uni
068700020423     c                   if        v1cpdr = *blanks
068800020423     c                   movel     *all'0'       f50pd1
068900020423     c                   else
069000020417     C                   movel     v1cpdr        f50pd1
069100020423     c                   end
069200020423     c                   if        v2cpdr = *blanks
069300020502     c                   movel     *all'9'       f50pd2
069400020423     c                   else
069500020411     C                   movel     V2CPDR        f50pd2
069600020423     c                   end
069700031121     C                   movel     causale       f50cau
069701031121     C                   movel     v1dpdr        f50dp1
069800020417     C                   movel     V2dPDR        f50dp2
069900020404     C                   Z-ADD     WDATDA        f50DTI
070000020404     C                   Z-ADD     WDATAA        f50DTF
070100020404     C                   Z-ADD     WDATFA        f50DFT
070200020404     C                   Z-ADD     V1CNFT        f50NFT
070300020404     C                   Z-ADD     WDATPR        f50DPR
070400980910     C                   MOVEL     KPJBU         dKPJBU          256
070500020419     C                   MOVEL(p)  ficn50ds      KPJBU
070600030128     c* AUTOTRASPORTATORI
070700080103     C                   CALL      'TNVRFICN1R'
070800951006     C                   PARM                    KPJBA
070900020404     C                   MOVEL     KPJBU         ficn50ds
071000980406     C*  Reimposta KPjBU
071100980406     C                   MOVEL     DKPJBU        KPJBU
071400951006      * Se premuto F3 vado a fine lavoro
071500020404     C     f50CMD        IFEQ      '03'
071600951006     C                   MOVEL     'S'           WFINE
071700951006     C                   ELSE
071800020404     C     f50CMD        IFNE      '12'
071900951006     C                   EXSR      INZ01
072000951006     C                   END
072100960402     C                   END
072200951006      *
072300951006     C                   ENDSR
072400980910     C*----------------------------------------------------*
072500980910     C* Reperimento dati società
072600980910     C*----------------------------------------------------*
072700980910     C     REPSOC        BEGSR
072800980910     C*
072900980910     C                   CALLB     'XSOC'
073000980910     C                   PARM                    TIPXSC            6
073100980910     C                   PARM                    SOCXSC            3
073200980910     C                   PARM                    CDSXSC            9 0
073300980910     C                   PARM                    MODXSC            3
073400980910     C                   PARM      *blanks       RTNXSC            1
073500980910     C                   PARM                    XSOCDS
073600980910     C                   PARM                    KPJBA
073700980910     C*
073800980910     C                   ENDSR
073900951006?     *--------------------------------------------------------------*
074000951006?     * *INZSR: Operazioni iniziali                                  *
074100951006?     *--------------------------------------------------------------*
074200951009     C     *INZSR        BEGSR
074300951006      *
074400951006     C     *ENTRY        PLIST
074500951006     C                   PARM                    KPJBA
074600031121     c                   movel     kpjbu         dspar
074700030128     c* decodifica a video
074800030827     c                   select
074900030827     c                   when      tipo = 'A'
075000030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
075100030827     c                   when      tipo = 'C'
075200030128     c                   seton                                        60
075300030128     c                   eval      dectip = '   COOPERATIVE   '
075400030827     c                   when      tipo = 'D'
075500030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
075600030827     c                   endsl
075700980910     C*---------- RICERCA DITTA :
075800980910     C                   MOVEL     'SOC001'      TIPXSC
075900980910     C                   MOVEL     *BLANK        SOCXSC
076000980910     C                   EXSR      REPSOC
076100980910     C     RTNXSC        IFNE      '1'
076200980910     C                   MOVEL     XSOCDS        SOC001
076300980910     C                   MOVEL     xscrgs        RSUT             20
076400020417     c                   else
076500020417     c                   goto      fine
076600980910     c                   end
076700020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
076800020415     C                   movel     'F     '      $kcc              6
076900020415     C                   movel     *blanks       $ksc              8
077000020415     C                   callb     'XCAPCLIFOR'
077100020415     C                   parm                    xscsoc
077200020415     C                   parm                    $kcc
077300020415     C                   parm                    $ksc
077400020415     C                   movel     $kcc          forita            6
077500030128      *  lettura tabella BLC x autotrasportatori
077600030827     c                   select
077700030827     c                   when      tipo = 'A'
077800020415     c                   clear                   dblc
077900030128     c                   movel(p)  '1'           tbeke1
078000030128     c                   movel     'BLC'         tbecod
078100030128     c     ktbe          chain     tntbe01l
078200030128     c                   if        %found(tntbe01l)
078300020415     c                   movel     tbeuni        dblc
078400020412     c* data protocollo
078500020513     c                   move      §blcdtp       dataiso
078600020417     c                   move      dataiso       wdatpr            8 0
078700020513     c                   move      dataiso       dataeur
078800020513     c                   move      dataeur       v1cdpr
078900020408     c                   else
079000020415     c                   clear                   dblc
079100020412     c                   move      0             v1cdpr
079200020408     c                   end
079300030128      *  lettura tabella BLO x cooperative
079400030827     c                   when      tipo = 'C'
079500030128     c                   clear                   dblo
079600030128     c                   movel(p)  '1'           tbeke1
079700030128     c                   movel     'BLO'         tbecod
079800030128     c     ktbe          chain     tntbe01l
079900030128     c                   if        %found(tntbe01l)
080000030128     c                   movel     tbeuni        dblo
080100030128     c* data protocollo
080200030128     c                   move      §blodtp       dataiso
080300030128     c                   move      dataiso       wdatpr            8 0
080400030128     c                   move      dataiso       dataeur
080500030128     c                   move      dataeur       v1cdpr
080600030128     c                   else
080700030128     c                   clear                   dblo
080800030128     c                   move      0             v1cdpr
080900030128     c                   end
081000030827      *  lettura tabella BLD x aff/defl.
081100030827     c                   when      tipo = 'D'
081200030827     c                   clear                   dbld
081300030827     c                   movel(p)  '1'           tbeke1
081400030827     c                   movel     'BLD'         tbecod
081500030827     c     ktbe          chain     tntbe01l
081600030827     c                   if        %found(tntbe01l)
081700030827     c                   movel     tbeuni        dbld
081800030827     c* data protocollo
081900030827     c                   move      §blddtp       dataiso
082000030827     c                   move      dataiso       wdatpr            8 0
082100030827     c                   move      dataiso       dataeur
082200030827     c                   move      dataeur       v1cdpr
082300030827     c                   else
082400030827     c                   clear                   dbld
082500030827     c                   move      0             v1cdpr
082600030827     c                   end
082700030827     c                   endsl
082800020423     C     Kuni          KLIST
082900020417     C                   KFLD                    xscsoc
083000020423     C                   KFLD                    v1cuni
083100020423     C     Kfrn          KLIST
083200020423     C                   KFLD                    xscsoc
083300020423     C                   KFLD                    kksc
083400980910     C     Krco          KLIST
083500020417     C                   KFLD                    xscsoc
083600020417     C                   KFLD                    forita
083700980910     C                   KFLD                    KKSC
083800030204     C     Kcdp          KLIST
083900030204     C                   KFLD                    xscsoc
084000030204     C                   KFLD                    condpv
084100020724     C     Ktbe1         KLIST
084200020724     C                   KFLD                    tbecod
084300020724     C                   KFLD                    tbeke1
084400020724     C                   KFLD                    tbeke2
084500020415     C     Ktbe          KLIST
084600020415     C                   KFLD                    tbecod
084700020415     C                   KFLD                    tbeke1
084800020510     C     Ktlz          KLIST
084900020510     C                   KFLD                    tlztip
085000020510     C                   KFLD                    tlzpdr
085100020510     C                   KFLD                    xscsoc
085200951006      *  Definisco variabili
085300980910     C                   Z-ADD     0             CURR              2 0
085400980910     C                   Z-ADD     0             CURC              2 0
085500951006      *  Inizializzo variabili
085600951006     C                   MOVEL     'N'           WFINE             1
085700980910      *
085800980910     c                   endsr
085900951006      *--------------------------------------------------------------*
086000951012** ERR
086100020404Codice fornitore obbligatorio                                         01
086200020404Codice fornitore inesiste o annullato                                 02
086300020415Codice digitato non appartenente ad un fornitore-autotrasportatore    03
086400951006Data obbligatoria                                                     04
086500951006Data errata                                                           05
086600951006Limiti incompatibili: data limite iniziale > finale                   06
086700951006Numero fattura obbligatorio                                           07
086800020404Data protocollo inferiore alla data bollato                           08
086900020404La data protocollo non può essere inferiore alla data fattura         09
087000020411Non esistono autotrasportatori con chiave alfabetica rischiesta       10
087100020529Se inserito documento fornitore è obbligatorio un codice fornitore    11
087200020408La data protocollo non può essere inferiore alla data finale          12
087300020408Impossibile contabilizzare: esite il blocco nella tabella BLC         13
087400020724Non esiste la tabella YIV                                             14
087500020423Unità di registrazione mancante/errata                                15
087600020423Unità di registrazione del fornitore è diversa da quella scelta       16
087700020423Utente non abilitato alla gestione dell'unità di registrazione        17
087800020503Codice iniziale maggiore del codice finale                            18
087900020509Incongruenza: per il fornitore esiste una numerazione automatica      19
088000020509Fornitore non abilitato all'uso di una propria numerazione            20
088100020515Fornitore abilitato all'uso di una propria numerazione                21
088200030128Impossibile contabilizzare: esite il blocco nella tabella BLO         22
088300030204Condizioni di pagamento errate                                        23
088400030827Impossibile contabilizzare: esite il blocco nella tabella BLD         24
