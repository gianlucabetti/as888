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
005300121031      *---
005400121031     D savpjbu         S                   like(KPJBU)
005500121031     D ficng5DS      E DS
005600121031      *---
005700980910     D* Definizione DS per recupero msg generati dai moduli
005800980910     D QLenMsg         S              9B 0
005900980910     D QFormat         S              8
006000980910     D QCallMsg        S             10
006100980910     D QCallStack      S              9B 0
006200980910     D QMsgTyp         S             10
006300980910     D QMsgKey         S              4
006400980910     D QWait           S              9B 0
006500980910     D QAction         S             10
006600980910     D QErrCod         DS
006700980910     D  Q1ErrLen                      9B 0 INZ(16)
006800980910     D  Q1LenRet                      9B 0 INZ(16)
006900980910     D  Q1ErrId                       7
007000980910     D*-------
007100980910     D QInfo           DS
007200980910     D  Q2BytRet                      9B 0
007300980910     D  Q2BytAva                      9B 0
007400980910     D  Q2Severity                    9B 0
007500980910     D  Q2MsgId                       7
007600980910     D  Q2MsgTyp                      2
007700980910     D  Q2MsgKey                      4
007800980910     D  Q2Reserve                    15
007900980910     D  Q2MsgLen                      9B 0
008000980910     D  Q2MsgLenAv                    9B 0
008100980910     D  Q2MsgTxt                     80
008200980910?     *--------------------------------------------------------------*
008300980910?     *  DS                                                          *
008400980910?     *--------------------------------------------------------------*
008500980910     D DSFMT           DS
008600980910     D  $TASTO               369    369
008700980910     D  NRG                  370    370
008800980910     D  NCL                  371    371
008900980910     D*-------------
009000980910     D* posizione cursore
009100980910     D CURSOR          DS
009200980910     D  RIRI                   1      2B 0 INZ
009300980910     D  R$$                    2      2
009400980910     D  COCO                   3      4B 0 INZ
009500980910     D  C$$                    4      4
009600980910     D*-------------
009700980910     D* Reperimento nome PGM
009800020510     D                SDS
009900020510     D  nompgm           *PROC
010000951002     D KPJBA         E DS
010100020722     D* LIBAZP               247    256
010200020722     D* LIBGES               257    266
010300020404     Ddtfmese                          d   datfmt(*iso)
010400020404     Ddataiso                          d   datfmt(*iso)
010500020415     Doggiiso                          d   datfmt(*iso)
010600980910     Ddataeur                          d   datfmt(*eur)
010700020417     Dkksc             s                   like(v1cpdr)
010800980911     D WData0          C                   CONST('0001-01-01')
010900020403     D ficn50ds      E DS
011000081124     D ficnB30DS     E DS
011100020404     D A009DS        E DS                  EXTNAME(ANA009DS) INZ
011200020423     D XAUTUNIDS     e ds                  inz
011300020415     D dblc          E DS
011400030128     D dblo          E DS
011500030827     D dbld          E DS
011600020513     D dtlz01        E DS
011700030206     D ANA024DS      E DS
011800031121     c* parametri entrata
011900031121     D dspar           DS
012000031121     D  tipo                          1
012100031121     D  causale                       4
012200960215     D*------------------
012300020403     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
012400960215     D*------------------
012500960215     D PARAM           DS                  INZ
012600020415     D* TIPOLOGIA P_=autotrasport.
012700000811     D  PARTIP                 1      2
012800020415     D* RAGIONE SOCIALE aoutotras.
012900000811     D  PARRSC                 3     37
013000020415     D* S.I.
013100000811     D  PARCSF                38     40
013200020415     D* CODICE
013300000811     D  PARPDR                41     47  0
013400020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
013500000811     D*                  ALFABETICA RICHIESTA
013600000811     D  PARFLG                48     48
013700980910     D soc001        E DS                  EXTNAME(xsoc001ds)
013800980910     D xsocds          DS          1000
013900990506     D*-------------
014000020403     D com13           s             13    inz('0000000000000')
014100990506     D*  Prototipazione delle procedure
014200990506     D/COPY *LIBL/SRCCPY,PJX002PR
014300951002?     *--------------------------------------------------------------*
014400951002?     *  CICLO PRINCIPALE                                            *
014500951002?     *--------------------------------------------------------------*
014600951002      *  Impostazione parametri prima videata
014700951002     C                   EXSR      INZ01
014800951002      *  Loop gestione videata
014900951002     C     WFINE         DOUEQ     'S'
015000951002     C                   EXSR      GESD01
015100951002     C                   END
015200951002      *  Fine Lavoro
015300951002     C     FINE          TAG
015400951002     C                   SETON                                        LR
015500951002?     *--------------------------------------------------------------*
015600951002?     *  INZ01: Inizializzazione prima videata                       *
015700951002?     *--------------------------------------------------------------*
015800951002     C     INZ01         BEGSR
015900951002      *
016000020403     C                   SETOFF                                       2898
016100020403     C                   movea     com13         *in(40)
016200020411     C                   movel     *blanks       V1CPDR
016300020411     C                   movel     *blanks       V2CPDR
016400951002     C                   Z-ADD     0             V1CDFT
016500951002     C                   Z-ADD     0             V1CNFT
016600030620     c* per le cooperative devo fatturare solo il mese
016700030620     c                   if        tipo = 'A'
016800020509     C                   Z-ADD     0             V1Cdti
016900030620     c                   else
017000030620     C                   Z-ADD     v1cdpr        V1Cdti
017100030620     C                   movel     01            V1Cdti
017200030620     c                   end
017300020509     C                   Z-ADD     v1cdpr        V1Cdtf
017400020404     C                   MOVEL     *BLANKS       V1DPDR
017500020403     C                   MOVEL     *BLANKS       V2DPDR
017600030128     C                   MOVEL     'S'           tsrv
017700030204     C                   MOVEL     *BLANKS       condpv
017800030204     C                   MOVEL     *BLANKS       descon
017900951002     C*
018000951002     C                   ENDSR
018100951002?     *--------------------------------------------------------------*
018200951002?     *  GESD01: Gestione prima videata                              *
018300951002?     *--------------------------------------------------------------*
018400951002     C     GESD01        BEGSR
018500951002      *
018600980910     C                   EXFMT     y350d01
018700121031      *
018800020403     C                   SETOFF                                       2898
018900020403     C                   movea     com13         *in(40)
019000020306     c                   movea     '00000000'    *in(71)
019100951002      *  Fine Lavoro
019200951002     C     *INKC         IFEQ      '1'
019300951002     C                   MOVEL     'S'           WFINE
019400951002     C                   GOTO      FINVD1
019500951002     C                   END
019600020403     C*  effettua ricerche per fornitore
019700980910     c                   if        *inkd = *on
019800980910     c                   exsr      search
019900020503     C                   GOTO      FINVD1
020000980910     c                   end
020100951002      *  Controlli
020200951002     C                   EXSR      CTR01
020300951002     C   28              GOTO      FINVD1
020400121108     C*
020500121108     C*  esegue controlli sulle FIRME  Bloccando x TIPO di contabilizzazione
020600121109     C*   Se NON si è FORZATO il BLOCCO con F8
020700140124     C* NO PER PRESTAZIONI OCCASIONALI
020800140124     C                   IF        CAUSALE = *BLANKS
020900121108     C                   EXSR      Ctrl_FIRME
021000121108     c     Devo_BLOCCARE CabEQ     'S'           finVD1
021100121108     C*
021200121031     C*  esegue controlli sulle FIRME  senza bloccare la contabilizzazione
021300121108     C                   EXSR      Ctrl_FIRME_GG
021400140124     C                   END
021500121031     C*
021600951002      *  No errori: call TNTL41R
021700020404     C                   EXSR      CALL51
021800951002      *
021900951002     C     FINVD1        ENDSR
022000951002?     *--------------------------------------------------------------*
022100951002?     *  CTR01: Controlli prima videata                              *
022200951002?     *--------------------------------------------------------------*
022300951002     C     CTR01         BEGSR
022400020423     C* Verifico unità di registrazione
022500020423     c                   movel     *blanks       v1cdun
022600020423     c                   if        v1cuni = *blanks
022700020423     C                   SETON                                        4628
022800020423     C                   MOVEL     ERR(15)       $MSG
022900020423     C                   GOTO      FINCT1
023000020423     c                   else
023100020423     c     kuni          chain     anuni01l                           46
023200020423     c                   if        not *in46
023300020423     c                   movel     unidesbrev    v1cdun
023400020423     C* CONTROLLO SE L'UTENTE E' ABILITATO AD USARE IL CDC
023500020423     C                   CLEAR                   XAUTUNIDS
023600020423     C                   MOVEL     KNMUS         XAUPRF
023700020423     C                   MOVEL     XSCSOC        XAUSOC
023800020423     C                   MOVEL     V1cuni        XAUUNI
023900020423     C                   CALL      'XAUTUNI'
024000020423     C                   PARM                    XAUTUNIDS
024100020423     C                   IF        XAUABL = '0'
024200020423     C                   SETON                                        4628
024300020423     C                   MOVEL     ERR(17)       $MSG
024400020423     C                   GOTO      FINCT1
024500020423     C                   END
024600020423     c                   else
024700020423     C                   SETON                                        28
024800020423     C                   MOVEL     ERR(15)       $MSG
024900020423     C                   GOTO      FINCT1
025000020423     c                   end
025100020423     c                   end
025200020724     C* Verifico esistenza tabella YIV
025300020724     C                   eval      tbecod = 'YIV'
025400020724     C                   eval      tbeke1 = xscsoc
025500020724     C                   eval      tbeke2 = v1cuni
025600020724     C     ktbe1         chain     tntbe01l
025700020724     C                   if        not %found(tntbe01l)
025800020423     C                   SETON                                        4628
025900020423     C                   MOVEL     ERR(14)       $MSG
026000020423     C                   GOTO      FINCT1
026100020423     C                   endif
026200020408     C* CONTROLLO se posso lanciare la contabilizzazione
026300030827     c                   select
026400030827     c                   when      tipo = 'A'
026500020513     c                   move      §blcdbl       dataiso
026600030827     c                   when      tipo = 'C'
026700030128     c                   move      §blodbl       dataiso
026800030827     c                   when      tipo = 'D'
026900030827     c                   move      §blddbl       dataiso
027000030827     c                   endsl
027100020415     c                   move      *date         dataeur
027200020415     c                   move      dataeur       oggiiso
027300020415     c                   if        oggiiso > dataiso
027400020408     c                   seton                                        28
027500030827     c                   select
027600030827     c                   when      tipo = 'A'
027700020408     C                   MOVEL     ERR(13)       $MSG
027800030827     c                   when      tipo = 'C'
027900030128     C                   MOVEL     ERR(22)       $MSG
028000030827     c                   when      tipo = 'D'
028100030827     C                   MOVEL     ERR(24)       $MSG
028200030827     c                   endsl
028300020408     C                   GOTO      FINCT1
028400020408     c                   end
028500020404      *  Controllo codice fornitore   dal
028600020503     C                   clear                   v1dpdr
028700020423     C     V1CPDR        IFne      *blanks
028800020411     C                   move      V1CPDR        kksc
028900980910     c                   movel     *blank        sogdes
029000980910     C     Kfrn          CHAIN     anfrn01l                           30
029100980910     C  N30Krco          CHAIN     anrco01l                           30
029200980910     C  N30rcosogg       CHAIN     ansog01l                           30
029300020403     C                   MOVEL     sogdes        f50DP1
029400980911     C                   MOVEL(p)  sogdes        v1dpdr
029500951002     C     *IN30         IFEQ      '1'
029600951002     C                   SETON                                        4028
029700951002     C                   MOVEL     ERR(2)        $MSG
029800951009     C                   GOTO      FINCT1
029900951002     C                   END
030000030210     C                   if        TIPO = 'A' and
030100030210     C                             rcoctgan02 <> 'N   ' and
030200020527     C                             rcoctgan02 <> 'I   ' and
030300020527     C                             rcoctgan02 <> 'M   '
030400980910     C                   SETON                                        4028
030500980910     C                   MOVEL     ERR(3)        $MSG
030600980910     C                   GOTO      FINCT1
030700980910     c                   end
030800020423     C                   if        rcounita <> v1cuni
030900020423     C                   SETON                                        4028
031000020423     C                   MOVEL     ERR(16)       $MSG
031100020423     C                   GOTO      FINCT1
031200020423     c                   end
031300020423     c                   end
031400020404      *  Controllo codice fornitore   al
031500020503     C                   clear                   v2dpdr
031600020411     C     V2CPDR        IFEQ      *blanks
031700020411     c                   movel     v1cpdr        v2cpdr
031800020411     C                   END
031900020423     C     V2CPDR        IFne      *blanks
032000020411     C                   move      V2CPDR        kksc
032100020403     c                   movel     *blank        sogdes
032200020403     C     Kfrn          CHAIN     anfrn01l                           30
032300020403     C  N30Krco          CHAIN     anrco01l                           30
032400020403     C  N30rcosogg       CHAIN     ansog01l                           30
032500020403     C                   MOVEL     sogdes        f50DP2
032600020403     C                   MOVEL(p)  sogdes        v2dpdr
032700020403     C     *IN30         IFEQ      '1'
032800020410     C                   SETON                                        4228
032900020403     C                   MOVEL     ERR(2)        $MSG
033000020403     C                   GOTO      FINCT1
033100020403     C                   END
033200030210     C                   if        tipo = 'A' and
033300030210     C                             rcoctgan02 <> 'N   ' and
033400020527     C                             rcoctgan02 <> 'I   ' and
033500020527     C                             rcoctgan02 <> 'M   '
033600020410     C                   SETON                                        4228
033700020403     C                   MOVEL     ERR(3)        $MSG
033800020403     C                   GOTO      FINCT1
033900020403     c                   end
034000020423     C                   if        rcounita <> v1cuni
034100020423     C                   SETON                                        4228
034200020423     C                   MOVEL     ERR(16)       $MSG
034300020423     C                   GOTO      FINCT1
034400020423     c                   end
034500020423     c                   end
034600020411     C*
034700020411     c                   if        v1cpdr > v2cpdr
034800020503     C                   MOVEL     ERR(18)       $MSG
034900020411     C                   SETON                                        404228
035000020411     C                   GOTO      FINCT1
035100020411     c                   end
035200020403     c* data da
035300020404     c                   if        v1cdti <> 0
035400980911     c     *eur          test(d)                 v1cdti                 41
035500980911     c                   if        *in41
035600980911     c     *dmy          test(d)                 v1cdti                 41
035700980911     c  n41*dmy          move      v1cdti        dataeur
035800980911     c  n41              move      dataeur       v1cdti
035900980911     c                   end
036000020404     c                   end
036100020403     c* data al
036200020404     c                   if        v1cdtf <> 0
036300020410     c     *eur          test(d)                 v1cdtf                 45
036400020415     c                   if        *in45
036500020410     c     *dmy          test(d)                 v1cdtf                 45
036600020415     c  n45*dmy          move      v1cdtf        dataeur
036700020415     c  n45              move      dataeur       v1cdtf
036800980911     c                   end
036900020404     c                   else
037000020410     c                   seton                                          2845
037100020404     c                   movel     err(4)        $msg
037200020404     c                   goto      finct1
037300020404     c                   end
037400020415     c                   if        *in41 or *in45
037500980910     c                   seton                                          28
037600980910     c                   movel     err(5)        $msg
037700980910     c                   goto      finct1
037800020404     c                   else
037900020404     c                   if        v1cdti <> 0
038000980910     c                   movel     v1cdti        dataeur
038100980910     c                   movel     dataeur       dataiso
038200980910     c                   movel     dataiso       wdatda            8 0
038300020404     c                   else
038400020404     c                   z-add     0             wdatda
038500020404     c                   end
038600980910     c                   movel     v1cdtf        dataeur
038700980910     c                   movel     dataeur       dataiso
038800980910     c                   movel     dataiso       wdataa            8 0
038900951002      *  Controllo limiti due date
039000951002     C     WDATDA        IFGT      WDATAA
039100020410     C                   SETON                                        414528
039200951002     C                   MOVEL     ERR(6)        $MSG                           Data iniz.>fina.
039300951002     C                   GOTO      FINCT1
039400951002     C                   END
039500980910     c                   end
039600951002      *  Controllo data Fattura
039700020403     C     V1CDFT        IFne      0
039800980911     c     *eur          test(d)                 v1cdft                 44
039900980911     c                   if        *in44
040000980911     c     *dmy          test(d)                 v1cdft                 44
040100980911     c  n44*dmy          move      v1cdft        dataeur
040200980911     c  n44              move      dataeur       v1cdft
040300980911     c                   end
040400980910     c                   if        *in44
040500980910     C                   SETON                                          28
040600951002     C                   MOVEL     ERR(5)        $MSG                           Data errata
040700951002     C                   GOTO      FINCT1
040800980910     c                   else
040900980910     c                   movel     v1cdft        dataeur
041000980910     c                   movel     dataeur       dataiso
041100980910     c                   movel     dataiso       wdatfa            8 0
041200020404     c                   end
041300020403     c                   if        v1cnft = 0
041400020403     C                   SETON                                        4328
041500020404     C                   MOVEL     ERR(7)        $MSG                           Data prot.<dat.fatt.
041600020403     C                   GOTO      FINCT1
041700020403     c                   end
041800020403     c                   else
041900020403     c                   if        v1cnft <> 0
042000020404     C                   SETON                                        4428
042100020404     C                   MOVEL     ERR(5)        $MSG                           Data prot.<dat.fatt.
042200020403     C                   GOTO      FINCT1
042300020403     c                   end
042400020404     c                   z-add     0             wdatfa
042500020403     c                   end
042600020411     c*
042700020509     c                   if        v1cnft <> 0
042800020509     c                   if        v1cpdr <> v2cpdr
042900020509     c                             or v1cpdr = *blanks
043000020509     C                   SETON                                        442843
043100020411     C                   MOVEL     ERR(11)       $MSG                           Data prot.<dat.fatt.
043200020411     C                   GOTO      FINCT1
043300020411     c                   end
043400020509     c* controllo se esiste un fornitore di fatturazione dal quale
043500020509     c* la numerazione fattura
043600080108     c*                  if        frnforfatt <> *blanks
043700080108     C*                  SETON                                        442843
043800080108     C*                  MOVEL     ERR(19)       $MSG                           Data prot.<dat.fatt.
043900080108     C*                  GOTO      FINCT1
044000080108     c*                  end
044100080108     c                   end
044200020515     c* controllo se il fornitore è abilitato all'uso della propria
044300020515     c* numerazione
044400020515     c                   move      v1cpdr        tlzpdr
044500030827     c                   select
044600030827     c                   when      tipo = 'A'
044700030211     c                   movel(p)  'P'           tlztip
044800030827     c                   when      tipo = 'C'
044900030211     c                   movel(p)  'C'           tlztip
045000030827     c                   when      tipo = 'D'
045100030827     c                   movel(p)  'D'           tlztip
045200030827     c                   endsl
045300020515     c     ktlz          chain     tntlz01l
045400030213     c                   if        %found(tntlz01l)
045500020515     c                   movel     tlzflo        dtlz01
045600030213     c                   if        §tlzfl1 = ' '
045700020515     c                   if        v1cnft <> 0
045800020515     C                   SETON                                        442843
045900020515     C                   MOVEL     ERR(20)       $MSG                           Data prot.<dat.fatt.
046000020515     C                   GOTO      FINCT1
046100020515     c                   end
046200030213     c                   else
046300020515     c                   if        v1cnft = 0
046400020515     C                   SETON                                        442843
046500020515     C                   MOVEL     ERR(21)       $MSG                           Data prot.<dat.fatt.
046600020515     C                   GOTO      FINCT1
046700020515     c                   end
046800030213     c                   end
046900030213     c                   if        tlzatb <> ' '
047000030211     C                   SETON                                        4228
047100030211     C                   MOVEL     ERR(2)        $MSG
047200030211     C                   GOTO      FINCT1
047300030213     c                   end
047400030213     c                   else
047500030213     c                   if        tipo = 'A' and v1cpdr <> *blanks
047600030211     C                   SETON                                        4228
047700030211     C                   MOVEL     ERR(2)        $MSG
047800030211     C                   GOTO      FINCT1
047900030211     c                   end
048000030211     c                   end
048100020404      *  Data Protocollo < data bollato
048200980910     c                   exsr      mvc104
048300980910     c                   if        esito = *on
048400980910     C                   GOTO      FINCT1
048500980910     c                   end
048600951011      *  Data Protocollo < data fattura
048700951011     C     WDATPR        IFLT      WDATFA
048800951011     C                   SETON                                        4828
048900020404     C                   MOVEL     ERR(9)        $MSG                           Data prot.<dat.fatt.
049000951011     C                   GOTO      FINCT1
049100951011     C                   END
049200020408      *  Data Protocollo < data finale
049300020408     C     WDATPR        IFLT      WDATaa
049400020410     C                   SETON                                        4528
049500020408     C                   MOVEL     ERR(12)       $MSG                           Data prot.<dat.fatt.
049600020408     C                   GOTO      FINCT1
049700020408     C                   END
049800030204      *  Condizioni di pagamento solo per Coop
049900030204     C                   IF        tipo = 'C'
050000030204     c                   CLEAR                   DESCON
050100030204     C                   IF        condpv <> *blanks
050200030204     c     kcdp          chain     ancdp01l
050300030204     c                   if        %found(ancdp01l)
050400030204     c                   movel     cdpdesbrev    descon
050500030204     C                   ELSE
050600030204     C                   SETON                                        4928
050700030204     C                   MOVEL     ERR(23)       $MSG                           Data prot.<dat.fatt.
050800030204     C                   GOTO      FINCT1
050900030204     C                   END
051000030204     C                   END
051100030204     C                   END
051200090825     c* per autististi verifico che sia stato inserito il prezzo carburante
051300111125     c*                  if        tipo = 'A' or tipo = 'D'
051400090825      *prelevo prezzo riferimento mese precedente
051500111125     c*                  move      wdatpr        dataiso
051600111125     c*    dataiso       subdur    1:*m          dataiso
051700111125     c*                  movel     dataiso       data8             8 0
051800111125     c*                  movel     data8         data6             6 0
051900111125     c*                  eval      tbecod = 'GPM'
052000111125     c*                  movel     data6         tbeke1
052100111125     c*                  eval      tbeke2 = *blank
052200111125     c*    ktbe1         chain     tntbe01l
052300111125     c*                  if        not %found(tntbe01l)
052400111125     C*                  SETON                                        4628
052500111125     C*                  eval      $MSG ='Manca tabella GPM. AVVISARE -
052600111125     c*                            il CED'
052700111125     C*                  GOTO      FINCT1
052800111125     c*                  endif
052900111125     c*                  end
053000081124     C* CONTROLLO SE CI SONO DISTINTE APERTE, BOLLE NON PARTITE, CONTEGGI
053100081124     C* NON VALORIZZATI SOLO PER AUT DI CITTÀ
053200140124     C                   IF        tipo = 'A' AND CAUSALE = *BLANKS
053300081124     C                   clear                   ficnb30ds
053400081124     c                   eval      CODPDD = 'N'
053500141105     c* deve controllare che siano tutte Confermate per poter fatturare.
053600141105     c                   eval      TIPO_VALOR='C'
053700081205     c* non controllo la massa perchè lo faccio dopo per ogni fornitore che
053800081205     c* che autofattura
053900081205     c                   eval      Ctrmassa = 'N'
054000081124     c                   movel     v1cuni        CODFILIALE
054100081205     c                   call      'FICNB30R'
054200081205     c                   parm                    ficnb30ds
054300081124     c                   if        TIPOERRORE <> ' '
054400081124     c                   movel     MSG_ERRORE    $msg
054500081124     C                   SETON                                        28
054600081124     c                   end
054700081124     c                   end
054800951003      *
054900951009     C     FINCT1        ENDSR
055000980910     C************************************************************
055100980910     C* Chiamata al modulo di controllo data registrazione
055200980910     C************************************************************
055300980910     C     MVC104        BEGSR
055400980910     C*
055500980910     C                   move      '046'         Unita
055600980911     C                   move      wdatpr        Dataiso
055700980911     C                   move      dataiso       DtReg
055800980911     C                   move      wdata0        Dtlgio
055900980910     C                   CALLB     'NDMVC104'
056000980910     C                   PARM      xscsoc        Societa
056100980910     C                   PARM      'CG'          CTB
056200980910     C                   PARM                    Unita
056300980910     C                   PARM      'D'           TpReg
056400980910     C                   PARM                    DTReg
056500980910     C                   PARM      '1'           RegIVA
056600980910     C                   PARM      '1'           TpRegIVA
056700980910     C                   PARM      '01'          Stato
056800980910     C                   PARM      *on           GesMsg
056900980910     C                   PARM                    Esito
057000980910     C*
057100980910     C* se ci sono errori
057200980910     C                   IF        Esito = *on
057300980910     C                   seton                                        48
057400980910     C* gestisco la loro emissione
057500980910     C                   DOU       Q2BytAva = 0
057600980910     C                   EXSR      RestoreMsg
057700980910     C                   IF        Q2BytAva > 0
057800980910     C                   EXSR      GestErr
057900980910     C                   ENDIF
058000980910     C                   ENDDO
058100980910     C*
058200980910     c                   else
058300980910     C                   CALLB     'NDMVC106'
058400980910    >C                   PARM                    Societa
058500980910    >C                   PARM                    Ctb
058600980910    >C                   PARM                    TpReg
058700980910    >C                   PARM                    DtLGio
058800980910    >C                   PARM                    DtReg
058900980910     C                   PARM                    GesMsg
059000980910     C                   PARM                    Esito
059100980910     c                   if        esito = *on
059200980910     C                   SETON                                        4828
059300020404     C                   MOVEL     ERR(8)        $MSG                           Data prot.<=bollato
059400980910     C                   END
059500980910     C                   ENDIF
059600980910     C*
059700980910     C*
059800980910     C                   ENDSR
059900980910     C************************************************************
060000980910     C* Recupero i msg sono stati generati dal modulo chiamato
060100980910     C************************************************************
060200980910     C     RestoreMsg    BEGSR
060300980910     C*
060400980910     C                   CALL      'QMHRCVPM'
060500980910     C                   PARM                    QInfo
060600980910     C                   PARM      113           QLenMsg
060700980910     C                   PARM      'RCVM0100'    QFormat
060800980910     C                   PARM      '*'           QCallMsg
060900980910     C                   PARM      0             QCallStack
061000980910     C                   PARM      '*DIAG'       QMsgTyp
061100980910     C                   PARM                    QMsgKey
061200980910     C                   PARM      0             QWait
061300980910     C                   PARM      '*REMOVE'     QAction
061400980910     C                   PARM                    QErrCod
061500980910     C*
061600980910     C                   ENDSR
061700980910     C************************************************************
061800980910     C* Gestione messaggi per il campo causale di testata
061900980910     C************************************************************
062000980910     C     GestErr       BEGSR
062100980910     C*
062200980910     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
062300980910     C                   MOVE      Q2MsgId       IdMsg
062400980910     C                   CALLB     'XRTVM'
062500980910     C                   PARM                    IDMSG            27
062600980910     C                   PARM                    TXTMSG          100
062700980910     C                   PARM                    ERRMSG            1
062800980910     C                   PARM                    *omit
062900980910     C                   PARM                    *omit
063000980910     C                   PARM      Q2msgtxt      Msgdta          100
063100980910     C                   IF        ErrMsg <> *on
063200020417     C                   eval      $msg = txtmsg
063300980910     C                   SETON                                        2848
063400980910     C                   ENDIF
063500980910     C*
063600980910     C                   ENDSR
063700020404     C************************************************************
063800020404     C* RICERCHE
063900020404     C************************************************************
064000020404     C     SEARCH        BEGSR
064100980910     C*
064200980910     C* determino Riga/Colonna del cursore
064300980910     C                   MOVE      nrg           R$$
064400980910     C                   MOVE      ncl           C$$
064500980910     C                   Z-ADD     RIRI          CURR
064600980910     C                   Z-ADD     COCO          CURC
064700980910     C*
064800980910     C* I campi che al momento dell'F4 risultano protetti non devono
064900980910     C* essere abilitati alla funzione di ricerca
065000980910     C*
0651009809101    C                   SELECT
065200020423     C* FMTD1  - ricerca unità di reg.
065300020423    >C     H1NMFL        WHEneq    'V1CUNI'
065400020423     C                   RESET                   A009DS
065500020423     C                   MOVE      XSCSOC        SOC009
065600020423     C                   EVAL      OPZ009 = '01'
065700020423     C                   EVAL      Kpjbu = A009DS
065800020423     C                   CALL      'ANA009R'
065900020423     C                   PARM                    KPJBA
066000020423     C                   EVAL      A009DS = Kpjbu
066100020423      *
066200020423     C     UNI009        IFNE      *BLANK
066300020423     C                   MOVE      UNI009        v1cuni
066400020423     C                   ENDIF
066500030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
066600030422    >C                   WHEn      H1NMFL = 'V1CPDR' AND
066700030422     c                             tipo = 'A'
066800980910     C*  deposita la KPJBU
066900980910     C                   MOVEL     KPJBU         DKPJBU          256
067000020503     C                   movel     xscsoc        parcsf
067100020404     C                   MOVEl     V1DPDR        PARRSC
067200980910     C                   Z-ADD     *ZEROS        PARPDR
067300020403     C                   eval      partip = 'P '
067400980910     C                   MOVEL     *BLANKS       PARFLG
067500020419     C                   MOVEL(p)  PARAM         KPJBU
067600980910     C                   CALL      'TRUL16R'
067700980910     C                   PARM                    KPJBA
067800980910     C                   MOVEL     KPJBU         PARAM
067900980910IF  2C     PARFLG        IFEQ      '3'
068000980910     C                   MOVEL     *BLANKS       V1DPDR
068100020403     C                   SETON                                        5028
068200020404     C                   MOVEL     ERR(10)       $MSG
068300980910X   2C                   ELSE
068400020422     C                   MOVEl     *zeros        V1CPDR
068500020422     C                   MOVE      PARPDR        V1CPDR
068600020403     C                   MOVEL     PARRSC        V1DPDR
068700980910E   2C                   ENDIF
068800030422     C* FMTD1  - ricerca fornitore 2 AUTOTRASPORTATRE
068900030422    >C                   WHEn      H1NMFL = 'V2CPDR' AND
069000030422     c                             tipo = 'A'
069100020403     C*  deposita la KPJBU
069200020403     C                   MOVEL     KPJBU         DKPJBU          256
069300020404     C                   MOVEl     V2DPDR        PARRSC
069400020403     C                   Z-ADD     *ZEROS        PARPDR
069500020403     C                   eval      partip = 'P '
069600020503     C                   movel     xscsoc        parcsf
069700020403     C                   MOVEL     *BLANKS       PARFLG
069800020419     C                   MOVEL(P)  PARAM         KPJBU
069900020403     C                   CALL      'TRUL16R'
070000020403     C                   PARM                    KPJBA
070100020403     C                   MOVEL     KPJBU         PARAM
070200020403IF  2C     PARFLG        IFEQ      '3'
070300020403     C                   MOVEL     *BLANKS       V2DPDR
070400020403     C                   SETON                                        5228
070500020404     C                   MOVEL     ERR(10)       $MSG
070600020403X   2C                   ELSE
070700020422     C                   MOVEl     *zeros        V2CPDR
070800020404     C                   MOVE      PARPDR        V2CPDR
070900020403     C                   MOVEL     PARRSC        V2DPDR
071000020403E   2C                   ENDIF
071100030206     C* FMTD1  - ricerca condizioni pagamento
071200030206    >C     H1NMFL        WHEneq    'CONDPV'
071300030206     C*  deposita la KPJBU
071400030206     C                   MOVEL     KPJBU         DKPJBU          256
071500030206     C                   RESET                   ANA024DS
071600030206     C                   MOVE      '01'          OPZ024
071700030206     C                   MOVEL(p)  ANA024DS      KPJBU
071800030206     C                   CALL      'ANA024R'
071900030206     C                   PARM                    KPJBA
072000030206     C                   MOVEL     KPJBU         ANA024DS
072100030206     C     Cod024        IFNE      *BLANK
072200030206     C                   MOVEL     Cod024        condpv
072300030206     c                   else
072400030206     c                   movel     *blanks       condpv
072500030206     C                   ENDIF
072600020510     C                   ENDsl
072700980910     C* REIMPOSTA LA KPJBU
072800980910     C                   MOVEL     DKPJBU        KPJBU
072900980910     C                   SETON                                        98
073000980910     C* imposto pos. del cursore
073100980910     C                   Z-ADD     CURR          H1RIGA
073200980910     C                   Z-ADD     CURC          H1COLO
073300980910     C                   ENDsr
073400951006?     *--------------------------------------------------------------*
073500020404?     *  CALL51: Controlli prima videata                             *
073600951006?     *--------------------------------------------------------------*
073700020404     C     CALL51        BEGSR
073800951006      *
073900020404     c                   clear                   ficn50ds
074000030128     C                   movel     tipo          f50tip
074100030204     C                   movel     condpv        f50cdp
074200030128     C                   movel     tsrv          f50tsr
074300030128     C                   movel     xscsoc        f50soc
074400020423     c                   movel     v1cuni        f50uni
074500020423     c                   if        v1cpdr = *blanks
074600020423     c                   movel     *all'0'       f50pd1
074700020423     c                   else
074800020417     C                   movel     v1cpdr        f50pd1
074900020423     c                   end
075000020423     c                   if        v2cpdr = *blanks
075100020502     c                   movel     *all'9'       f50pd2
075200020423     c                   else
075300020411     C                   movel     V2CPDR        f50pd2
075400020423     c                   end
075500031121     C                   movel     causale       f50cau
075600031121     C                   movel     v1dpdr        f50dp1
075700020417     C                   movel     V2dPDR        f50dp2
075800020404     C                   Z-ADD     WDATDA        f50DTI
075900020404     C                   Z-ADD     WDATAA        f50DTF
076000020404     C                   Z-ADD     WDATFA        f50DFT
076100020404     C                   Z-ADD     V1CNFT        f50NFT
076200020404     C                   Z-ADD     WDATPR        f50DPR
076300980910     C                   MOVEL     KPJBU         dKPJBU          256
076400020419     C                   MOVEL(p)  ficn50ds      KPJBU
076500030128     c* AUTOTRASPORTATORI
076600020404     C                   CALL      'FICN51R'
076700951006     C                   PARM                    KPJBA
076800020404     C                   MOVEL     KPJBU         ficn50ds
076900980406     C*  Reimposta KPjBU
077000980406     C                   MOVEL     DKPJBU        KPJBU
077100951006      * Se premuto F3 vado a fine lavoro
077200020404     C     f50CMD        IFEQ      '03'
077300951006     C                   MOVEL     'S'           WFINE
077400951006     C                   ELSE
077500020404     C     f50CMD        IFNE      '12'
077600951006     C                   EXSR      INZ01
077700951006     C                   END
077800960402     C                   END
077900951006      *
078000951006     C                   ENDSR
078100980910     C*----------------------------------------------------*
078200980910     C* Reperimento dati società
078300980910     C*----------------------------------------------------*
078400980910     C     REPSOC        BEGSR
078500980910     C*
078600980910     C                   CALLB     'XSOC'
078700980910     C                   PARM                    TIPXSC            6
078800980910     C                   PARM                    SOCXSC            3
078900980910     C                   PARM                    CDSXSC            9 0
079000980910     C                   PARM                    MODXSC            3
079100980910     C                   PARM      *blanks       RTNXSC            1
079200980910     C                   PARM                    XSOCDS
079300980910     C                   PARM                    KPJBA
079400980910     C*
079500980910     C                   ENDSR
079600951006?     *--------------------------------------------------------------*
079700951006?     * *INZSR: Operazioni iniziali                                  *
079800951006?     *--------------------------------------------------------------*
079900951009     C     *INZSR        BEGSR
080000951006      *
080100951006     C     *ENTRY        PLIST
080200951006     C                   PARM                    KPJBA
080300031121     c                   movel     kpjbu         dspar
080400030128     c* decodifica a video
080500030827     c                   select
080600030827     c                   when      tipo = 'A'
080700030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
080800030827     c                   when      tipo = 'C'
080900030128     c                   seton                                        60
081000030128     c                   eval      dectip = '   COOPERATIVE   '
081100030827     c                   when      tipo = 'D'
081200030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
081300030827     c                   endsl
081400980910     C*---------- RICERCA DITTA :
081500980910     C                   MOVEL     'SOC001'      TIPXSC
081600980910     C                   MOVEL     *BLANK        SOCXSC
081700980910     C                   EXSR      REPSOC
081800980910     C     RTNXSC        IFNE      '1'
081900980910     C                   MOVEL     XSOCDS        SOC001
082000980910     C                   MOVEL     xscrgs        RSUT             20
082100020417     c                   else
082200020417     c                   goto      fine
082300980910     c                   end
082400020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
082500020415     C                   movel     'F     '      $kcc              6
082600020415     C                   movel     *blanks       $ksc              8
082700020415     C                   callb     'XCAPCLIFOR'
082800020415     C                   parm                    xscsoc
082900020415     C                   parm                    $kcc
083000020415     C                   parm                    $ksc
083100020415     C                   movel     $kcc          forita            6
083200030128      *  lettura tabella BLC x autotrasportatori
083300030827     c                   select
083400030827     c                   when      tipo = 'A'
083500020415     c                   clear                   dblc
083600030128     c                   movel(p)  '1'           tbeke1
083700030128     c                   movel     'BLC'         tbecod
083800030128     c     ktbe          chain     tntbe01l
083900030128     c                   if        %found(tntbe01l)
084000020415     c                   movel     tbeuni        dblc
084100020412     c* data protocollo
084200020513     c                   move      §blcdtp       dataiso
084300020417     c                   move      dataiso       wdatpr            8 0
084400020513     c                   move      dataiso       dataeur
084500020513     c                   move      dataeur       v1cdpr
084600020408     c                   else
084700020415     c                   clear                   dblc
084800020412     c                   move      0             v1cdpr
084900020408     c                   end
085000030128      *  lettura tabella BLO x cooperative
085100030827     c                   when      tipo = 'C'
085200030128     c                   clear                   dblo
085300030128     c                   movel(p)  '1'           tbeke1
085400030128     c                   movel     'BLO'         tbecod
085500030128     c     ktbe          chain     tntbe01l
085600030128     c                   if        %found(tntbe01l)
085700030128     c                   movel     tbeuni        dblo
085800030128     c* data protocollo
085900030128     c                   move      §blodtp       dataiso
086000030128     c                   move      dataiso       wdatpr            8 0
086100030128     c                   move      dataiso       dataeur
086200030128     c                   move      dataeur       v1cdpr
086300030128     c                   else
086400030128     c                   clear                   dblo
086500030128     c                   move      0             v1cdpr
086600030128     c                   end
086700030827      *  lettura tabella BLD x aff/defl.
086800030827     c                   when      tipo = 'D'
086900030827     c                   clear                   dbld
087000030827     c                   movel(p)  '1'           tbeke1
087100030827     c                   movel     'BLD'         tbecod
087200030827     c     ktbe          chain     tntbe01l
087300030827     c                   if        %found(tntbe01l)
087400030827     c                   movel     tbeuni        dbld
087500030827     c* data protocollo
087600030827     c                   move      §blddtp       dataiso
087700030827     c                   move      dataiso       wdatpr            8 0
087800030827     c                   move      dataiso       dataeur
087900030827     c                   move      dataeur       v1cdpr
088000030827     c                   else
088100030827     c                   clear                   dbld
088200030827     c                   move      0             v1cdpr
088300030827     c                   end
088400030827     c                   endsl
088500020423     C     Kuni          KLIST
088600020417     C                   KFLD                    xscsoc
088700020423     C                   KFLD                    v1cuni
088800020423     C     Kfrn          KLIST
088900020423     C                   KFLD                    xscsoc
089000020423     C                   KFLD                    kksc
089100980910     C     Krco          KLIST
089200020417     C                   KFLD                    xscsoc
089300020417     C                   KFLD                    forita
089400980910     C                   KFLD                    KKSC
089500030204     C     Kcdp          KLIST
089600030204     C                   KFLD                    xscsoc
089700030204     C                   KFLD                    condpv
089800020724     C     Ktbe1         KLIST
089900020724     C                   KFLD                    tbecod
090000020724     C                   KFLD                    tbeke1
090100020724     C                   KFLD                    tbeke2
090200020415     C     Ktbe          KLIST
090300020415     C                   KFLD                    tbecod
090400020415     C                   KFLD                    tbeke1
090500020510     C     Ktlz          KLIST
090600020510     C                   KFLD                    tlztip
090700020510     C                   KFLD                    tlzpdr
090800020510     C                   KFLD                    xscsoc
090900951006      *  Definisco variabili
091000980910     C                   Z-ADD     0             CURR              2 0
091100980910     C                   Z-ADD     0             CURC              2 0
091200951006      *  Inizializzo variabili
091300951006     C                   MOVEL     'N'           WFINE             1
091400980910      *
091500980910     c                   endsr
091600121108     C* --------------------------------------------------------------
091700121108     C*--- CONTROLLO prima della Contabilizzazione del mese ----------*
091800121108     C* --------------------------------------------------------------
091900121108     C     CTRL_FIRME    BEGSR
092000121108     C*
092100121108     C*  esegue un controllo sulle Valorizzazioni Firmate x Tipologia Autista
092200121108     C*   CITTA', COOP e AFFLUENZE/DEFLUENZE
092300121108     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
092400121109     C*   viene emessa una Window di avvertimento con relativa Facoltà di avanzare
092500121109     C*   tramite Forzatura.
092600121108     c                   move      'N'           devo_bloccare     1
092700121108     C                   MOVEL     kpjbu         savpjbu
092800121108     C*
092900121108      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
093000121109      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/D=AFFDEF  <--
093100121108      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
093200121108      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
093300121108      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
093400121108     C*
093500121108     C                   clear                   kpjbu
093600121108     C                   clear                   ficng5ds
093700121108     c                   eval      §G5CNVL  = 'C'
093800121108     c                   eval      §G5TIPO  = Tipo
093900121108     c                   movel     v1cuni        §G5FIL
094000121108     C                   MOVEL     ficng5ds      kpjbu
094100121108     c                   call      'FICNG5R'
094200121108     C                   parm                    kpjba
094300121108     C                   MOVEL     kpjbu         ficng5ds
094400121108     C                   clear                   kpjbu
094500121108     C                   MOVEL     savpjbu       kpjbu
094600121108      **
094700121109      *  Il flag viene Ritornato come BLOCCANTE se NON si è FORZATO il controllo.
094800121109     C*   ed in questo caso il programma rimane fermo sulla videata senza Procedere.
094900121108     c                   if        §G5ESITO ='B'
095000121108     c                   eval        devo_bloccare = 'S'
095100121108     c                   end
095200121108     C*
095300121108     C                   ENDSR
095400121031     C* --------------------------------------------------------------
095500121031     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
095600121031     C* --------------------------------------------------------------
095700121108     C     CTRL_FIRME_GG BEGSR
095800121031     C*
095900121031     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
096000121031     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
096100121031     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
096200121109     C*   viene emessa una Window di avvertimento.
096300121031     c                   move      'N'           devo_bloccare     1
096400121031     C                   MOVEL     kpjbu         savpjbu
096500121031     C*
096600121031      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
096700121108      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/D=AFFDEF
096800121031      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
096900121031      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
097000121031      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
097100121031     C*
097200121031     C                   clear                   kpjbu
097300121031     C                   clear                   ficng5ds
097400121031     c                   eval      §G5CNVL  = 'C'
097500121031     c                   movel     v1cuni        §G5FIL
097600121031     C                   MOVEL     ficng5ds      kpjbu
097700121031     c                   call      'FICNG5R'
097800121031     C                   parm                    kpjba
097900121031     C                   MOVEL     kpjbu         ficng5ds
098000121031     C                   clear                   kpjbu
098100121031     C                   MOVEL     savpjbu       kpjbu
098200121031     C*
098300121109     C* COMUNQUE NON blocca nulla e il programma procede regolarmente
098400121031     C*
098500121031     C                   ENDSR
098600121031     C* --------------------------------------------------------------
098700951012** ERR
098800020404Codice fornitore obbligatorio                                         01
098900020404Codice fornitore inesiste o annullato                                 02
099000020415Codice digitato non appartenente ad un fornitore-autotrasportatore    03
099100951006Data obbligatoria                                                     04
099200951006Data errata                                                           05
099300951006Limiti incompatibili: data limite iniziale > finale                   06
099400951006Numero fattura obbligatorio                                           07
099500020404Data protocollo inferiore alla data bollato                           08
099600020404La data protocollo non può essere inferiore alla data fattura         09
099700020411Non esistono autotrasportatori con chiave alfabetica rischiesta       10
099800020529Se inserito documento fornitore è obbligatorio un codice fornitore    11
099900020408La data protocollo non può essere inferiore alla data finale          12
100000020408Impossibile contabilizzare: esite il blocco nella tabella BLC         13
100100020724Non esiste la tabella YIV                                             14
100200020423Unità di registrazione mancante/errata                                15
100300020423Unità di registrazione del fornitore è diversa da quella scelta       16
100400020423Utente non abilitato alla gestione dell'unità di registrazione        17
100500020503Codice iniziale maggiore del codice finale                            18
100600020509Incongruenza: per il fornitore esiste una numerazione automatica      19
100700020509Fornitore non abilitato all'uso di una propria numerazione            20
100800020515Fornitore abilitato all'uso di una propria numerazione                21
100900030128Impossibile contabilizzare: esite il blocco nella tabella BLO         22
101000030204Condizioni di pagamento errate                                        23
101100030827Impossibile contabilizzare: esite il blocco nella tabella BLD         24
