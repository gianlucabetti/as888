000001090204     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200060302     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400081209      *     - chiusura incassi prepagati/assegnati dopo chiusura op. *
000500960702     F*--------------------------------------------------------------*
000600081209     Fcnc0w8D   CF   E             WORKSTN
000700060224     FFNARB01L  UF   E           K DISK    commit
000800081209     FFiinc01L  uF a E           K DISK    commit
000902060414     Ffiari01L  uF   E           K DISK    commit
001100060224     FFnlbl01L  iF   E           K DISK
001101081209     FFnblp01L  iF   E           K DISK
001200030730     FTABEL00F  iF   E           K DISK
001300940321      *---------------------------------------------------------------
001400030730     d* schiere
001500030730      *---------------------------------------------------------------
001600030730     d* ds esterne
001700030730      *---------------------------------------------------------------
001800030730     D KPJBA         E DS
001900030730     D ddatiute      e ds
002000060301     D fnarbdssav    e ds                  extname(fnarb00F) prefix(s)
002100060301     D fnarbds       e ds                  extname(fnarb00F)
002101090204     D fiincds       e ds                  extname(fiinc00F)
002200060301     D fnarbmds      e ds                  extname(fnarb00F) prefix(m)
002300060301     D azuteds       e ds                  extname(AZUTE00F)
002400060309     D fiar6ds       e ds                  extname(fiar600F)
002500060309     D fiar9ds       e ds                  extname(fiar900F)
002600030730     D tibs34ds      E DS                  inz
002700060307     D ds3a          E DS                  inz
002800060307     D ds1a          E DS                  inz
002900060307     D ds4f          E DS                  inz
003300060224      * DS per richiamo pgm FNLR36R
003400060224     D DSLR36          DS
003500060224     D  P36AAS                14     17  0
003600060224     D  P36LNP                18     20  0
003700060224     D  P36NRS                21     22  0
003800060224     D  P36NSP                23     29  0
003900060224     D  P36F03                30     30
004000060224     D  P36FLG                31     31
004100030730     d dtaeur          s               d   datfmt(*eur)
004200030730     d dtaiso          s               d
004201060915     D dutpouc         s                   LIKE(dutpou)
004300060307     D chkBollaIncassata...
004400060307     D                 PR            10I 0
004500060307     D  aas                                LIKE(arbaas)
004600060307     D  lnp                                LIKE(arblnp)
004700060307     D  nrs                                LIKE(arbnrs)
004800060307     D  nsp                                LIKE(arbnsp)
004900060307     D  cbo                                LIKE(arbcbo)
005000060307     D                                     OPTIONS(*OMIT)
005100060307     D  datiCbo                      89A
005200060310     D                                     OPTIONS(*OMIT)
005300060307     D  tpd                           1A
005400060307     D  esito                        10I 0
005500060307     D  err                           1A   DIM(10)
005600060307     D  msg                          80A   DIM(10)
005700060307     D                                     OPTIONS(*OMIT)
005800060310     D  datiFattura               32767A
005900060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006000060307     D                                     VARYING
006100060310     D  datiContrassegno...
006200060307     D                            32767A
006300060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006400060307     D                                     VARYING
006500060307     D chkIncassoPO    PR            10I 0
006600060321     D  soc                           3a
006700060321     D                                     CONST
006800060321     D  fgs                           3  0
006900060307     D  tic                                LIKE(inctii)
007000060307     D  tpd                           1A   CONST
007100060307     D  dati1a                             LIKE(tblUni)
007200060307     D                                     OPTIONS(*OMIT)
007300060321     D  datiY4O                     256A
007400060321     D                                     OPTIONS(*OMIT)
007500090204     D  nra                                LIKE(incnra)
007600060307     D                                     OPTIONS(*OMIT)
007700090204     D  abi                                LIKE(incabi)
007800060307     D                                     OPTIONS(*OMIT)
007900090204     D  cab                                LIKE(inccab)
008000060307     D                                     OPTIONS(*OMIT)
008100060307     D  dte                          10D   DATFMT(*ISO)
008200060307     D                                     OPTIONS(*OMIT)
008300060307     D                                     CONST
008400060307     D  esito                        10I 0
008500060307     D                                     OPTIONS(*OMIT)
008600060307     D  err                           1A
008700060307     D                                     DIM(10)
008800060307     D  msg                          80A   OPTIONS(*OMIT)
008900060307     D                                     DIM(10)
009000060321     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
009100060321     D                                     LIKEDS(kpjba)
009200060307     D prmEsito        S             10I 0
009300060307     D esito           S             10I 0
009400060307     D err             S              1    DIM(10)                              SKI CHIAVI
009500060310     D msg             S             80    DIM(10)                              SKI CHIAVI
009600060307     D C3A             S              2    DIM(100)                             SKI CHIAVI
009700060310     D D3A             S                   like(tbluni) DIM(100)                SKI TAB. 3
009800060321     D fgs             S                   like(arblnp)                         SKI TAB. 3
009900090204     D nra             S                   like(incnra)                         SKI TAB. 3
010000060307     D kaas            S                   like(arbaas)                         SKI TAB. 3
010100060307     D klnp            S                   like(arblnp)                         SKI TAB. 3
010200060307     D knrs            S                   like(arbnrs)                         SKI TAB. 3
010300060307     D knsp            S                   like(arbnsp)                         SKI TAB. 3
010400090204     D savabi          S                   like(incabi)                         SKI TAB. 3
010500090204     D savcab          S                   like(inccab)                         SKI TAB. 3
010600060310     Ddatifat          s          32767A
010700060310     D                                     VARYING
010800060310     Ddatica           s          32767A
010900060310     D                                     VARYING
011000911016      *---------------------------------------------------------------*
011100911016      * FLUSSO PRINCIPALE                                             *
011200911016      *---------------------------------------------------------------*
011201090204     c                   exsr      srinz
011300030730     C                   DO        *HIVAL
011400030730      *
011500060308     c                   if        *in90
011600060308     c                   setoff                                       90
011700030730     C                   write     LR98D01
011800030730     c                   seton                                        90
011900060308     c                   end
012000030730     c*
012100030730     C                   EXFMT     LR98D01
012200030730     c* fine lavoro
012300941107     C     *INKC         IFEQ      '1'
012400030730     C                   LEAVE
012500941107     C                   END
012600030730     c* controlli
012700030730     c                   exsr      srctr
012800060412     c   kc              leave
012801060412     c   90              iter
013400030730     c*aggiorna
013500060302     c                   if        *inkf
013600030730     c                   exsr      sraggio
013700030730     c   90              iter
013800030730     c*inizializza
013900030730     c                   exsr      srinz
014000030730     c                   end
014100030730     c                   enddo
014200941107     C*
014300030730     C                   SETON                                        LR
014400030730     c**********************************************************************
014500030730     C     srinz         BEGSR
014600030730     c**********************************************************************
014700060224     C* Pulisco i campi che devo visualizzare nel fm2
014701090204     c                   eval      v03tdc = 'A'
014800060301     C                   MOVE      *year         V03AAS
014900060224     C                   clear                   V03LNP
015000060224     C                   clear                   V03NRS
015100060224     C                   clear                   V03NSP
016900030730     c                   endsr
017000030730     c**********************************************************************
017100030730     C     srctr         BEGSR
017200030730     c**********************************************************************
017300030620     C                   SETOFF                                       90
017301060412     c                   clear                   v03msg
017400030730     c                   do
017500030730     C*F7-INTERROGAZIONE BOLLE ARRIVI
017600030730     C                   if        *inkg
017700941109     C                   MOVEL     '2'           P36FLG
017800060301     C                   Z-ADD     V03AAS        P36AAS
017900941108     C                   Z-ADD     V03LNP        P36LNP
018000941108     C                   Z-ADD     V03NSP        P36NSP
018100941108     C                   Z-ADD     V03NRS        P36NRS
018200941109     C                   MOVEL     ' '           P36F03
018300941109     C     P36NSP        IFGT      0
018400941109     C                   MOVEL     '1'           P36FLG
018500941109     C                   END
018600941108     C                   MOVEL     DSLR36        KPJBU
018700941108     C                   CALL      'FNLR36R'
018800911128     C                   PARM                    KPJBA
018900941109     C                   MOVEL     KPJBU         DSLR36
019000941109     C     P36F03        IFNE      '1'
019100941215     C     V03NSP        ANDEQ     0
019200060301     C                   Z-ADD     P36AAS        V03AAS
019300941109     C                   Z-ADD     P36LNP        V03LNP
019400941109     C                   Z-ADD     P36NRS        V03NRS
019500941109     C                   Z-ADD     P36NSP        V03NSP
019600941109     C                   END
019700911128     C                   SETON                                        90
019800030730     c                   leave
019900911128     C                   END
020400060307     C* tipo documento
020600060321     c                   setoff                                       15
020700090204     C     V03tdc        IFEQ      *blanks
020900060307     C                   SETON                                        1590
021000030730     c                   leave
021001090204     c                   end
022300060307     C* Controllo se la spedizione è esistente o valida su video
022400060307     C     V03AAS        IFEQ      *ZEROS
022500060307     C                   SETON                                        0190
022600060307     c                   leave
022700060307     C                   END
022800911017     C     V03LNP        IFEQ      *ZEROS
022900030730     C                   SETON                                        0290
023000030730     c                   leave
023100911017     C                   END
023200911017     C     V03NSP        IFEQ      *ZEROS
023300030730     C                   SETON                                        0390
023400030730     c                   leave
023500911017     C                   END
023600081209     C* AGGANCIO FIAR6
023700060301     C                   Z-ADD     v03aas        KAAS
023800060301     C                   Z-ADD     v03lnp        KLNP
023900060307     C                   Z-ADD     v03NRs        KNRS
024000060301     C                   Z-ADD     v03NSP        KNSP
024001081209     c*
024701081209     c                   if        v03tdc = 'A'
024702081209     c                   exsr      ctrdis
024801081209     c                   else
024802081209     c                   exsr      ctrpre
024803081209     c                   end
024804060412     c   90              leave
024901081209     c*
027206060412     c                   enddo
027300030730     c*
027400911017     C                   ENDSR
027972060412      *---------------------------------------------------------*
027973060412      *  Controllo c/ass. o assegnato                           *
027974060412      *---------------------------------------------------------*
027975060412     C     CTRDIS        BEGSR
027976090204     c                   setoff                                       90
027977060310     c                   clear                   v03msg
027979060412     c                   clear                   datifat
027980060412     c                   clear                   datica
027981081209     c                   do
027982081209     c* controllo spedizione
027984081209     C     Kspe          CHAIN(n)  FNARB01L
027985081209     c                   if        not %found(fnarb01l)
027986081209     c                   seton                                        0590
027987081209     c                   leave
027988081209     c                   else
027989081209     c                   movel     fnarbds       fnarbdssav
027990081209     c                   end
028100060228     C* controllo il mandato d'intrito
028200081209     c                   if        arbnmi = 0
028300060228     c                   seton                                        0690
028400060228     c                   leave
028500060228     c                   end
028501081210     c* assegnato incassato
028502081210     c                   if        arbica <> 'R'
028503081210     c                   seton                                        0790
028504081210     c                   leave
028505081210     C                   END
030400060302     C* Controllo se esiste bolla mamma
030500060310     c                   clear                   fnarbmds
030600060228     C     KSPE          CHAIN     FNLBL01L
030700060228     c* per cambio di porto il contrassegno è sulla mamma
030800060228     C                   IF        %found(fnlbl01l) and lbllan = lbllap
030900060224     C                   Z-ADD     LBLAAP        KAAS
031000060224     C                   Z-ADD     LBLLPP        KLNP
031100060224     C                   Z-ADD     LBLNRP        KNRS
031200060224     C                   Z-ADD     LBLNSP        KNSP
031300060301     C     KSPE          CHAIN(n)  FNARB01L
031400060224     C* Memorizzo il legame esistente per i successivi accessi su FNARB
031500060228     C                   IF        %found(fnarb01l) and arbcca <> '2' and
031600060228     C                             arbcca <> '6'
031700060301     C                   movel     fnarbds       fnarbmds
031800060224     C                   END
031900060301     C                   movel     fnarbdssav    fnarbds
032000060224     C                   END
032100081209     C* controllo se c'è la fattura
032200060309     C                   Z-ADD     1             X
032300060309     C                   CLEAR                   DS3A
032400060309     C     ARBCBO        LOOKUP    C3A(X)                                 95
032500060309     C     *IN95         IFEQ      '1'
032600060309     C                   MOVEL     D3A(X)        DS3A
032700060309     C                   END
032800060309     c                   eval      tbluni = ds3a
033501081209     c* se non sono previsti incassi con fattura immediata
033503081209     c                   if        §3ati1 <> 'A' and §3ati2 <> 'A'
033505060418     c                   seton                                        1690
033506060418     c                   leave
033507060418     c                   end
033600060309     c* controllo bolla
033900060307     c                   callp     CHKBOLLAincassata(
034000060307     c                             arbaas:arblnp:arbnrs:arbnsp
034100081210     c                             :arbcbo:tbluni:v03tdc:esito:err:msg
034200060310     c                             :datifat:datica)
034300090204     c* se errore oppure fattura sulla mamma
034400060321     c                   if        esito <>0 and marbaas <> 0
034500060309     c                   clear                   esito
034600060309     c                   callp     CHKBOLLAincassata(
034700060309     c                             marbaas:marblnp:marbnrs:marbnsp
034800081210     c                             :marbcbo:*omit:v03tdc:esito:err:msg
034900060310     c                             :*omit:datica)
035000060309     c                   end
035100060310     C*
0352000603101    c                   if        esito = 0
035300060310     c                   movel     datifat       fiar6ds
035301090204     c                   else
035302090204     c                   clear                   fiar6ds
035304090205     c                   do        10            xx                2 0
035305090204     c                   if        err(xx) = ' '
035306090204     c                   leave
035307090204     c                   end
035308090204     c                   movel     msg(xx)       v03msg
035309090204     c                   seton                                        90
035310090204     c                   leave
0353110902042    C                   ENDdo
035312090204     c   90              leave
035313090204     c                   end
035900090204     c* errore incasso o segue fattura
036000080522     c                   if        ar6dft = 0
036100080522     c                   seton                                        1690
036200060228     c                   leave
036300060227     C                   END
041035060412     c*
041036060412     C                   ENDdo
041100060224     C*
041200060224     C                   ENDSR
041300060228      *---------------------------------------------------------*
041400060228      *  Controllo prepagato                                    *
041500060228      *---------------------------------------------------------*
041600060228     C     CTRPRE        BEGSR
041700090204     c                   setoff                                       90
041800060308     c                   clear                   v03msg
041801090204     c                   clear                   datifat
041802090204     c                   clear                   datica
041900060228     c                   do
041907081210     c* controllo prepagato
041908090204     C     Kspe          CHAIN     FNBLP01L
041909081210     c                   if        not %found(fnBLP01l)
041910081209     c                   seton                                        0590
041911081209     c                   leave
041912081209     c                   end
043001081210     c* controllo se prepagato incassato
043002081210     C     Kspe          chain     Fiinc01L
043003090204     c                   if        %found(fiinc01l)
043004090204     c                   seton                                        9009
043005081210     c                   leave
043007081210     c                   end
043100060301     c* controllo spedizione
043200060307     c                   callp     CHKBOLLAincassata(
043300081210     c                             blpaas:blplnp:blpnrs:blpnsp
043400081210     c                             :*omit:*omit:v03tdc:esito:err:msg
043501090204     c                             :datifat:*omit)
0435020902041    c                   if        esito = 0
043503090204     c                   movel     datifat       fiar6ds
043504090204     c                   else
043505090204     c                   clear                   fiar6ds
043700060310     c                   do        10            xx
043800060310     c                   if        err(xx) = ' '
043900060310     c                   leave
044000060310     c                   end
044100060310     c                   movel     msg(xx)       v03msg
044200060310     c                   seton                                        90
044300060310     c                   leave
0444000603102    C                   ENDdo
044401090204     c   90              leave
044500060301     c                   end
044600090204     c* il pre-pagato non deve essere incassato
044601090205     c*                  clear                   incfiv
044602090205     c*                  clear                   incdft
044603090205     c*                  clear                   incnft
044604090205     c*                  clear                   prmesito
044700090205     c*                  call      'YCOW0121R'
044800090205     C*                  PARM                    blpAas
044900090205     C*                  PARM                    blpLnp
045000090205     C*                  PARM                    blpnrs
045100090205     C*                  PARM                    blpNsp
045200090205     C*                  PARM                    incFiv
045300090205     C*                  PARM                    incDft
045400090205     C*                  PARM                    incNft
045500090205     C*                  PARM                    prmEsito
045600090205     c*                  if        prmesito = 0
045700090205     C*                  SETON                                        09
045800090205     c*                  leave
045900090205     c*                  end
059300060228     C                   ENDdo
059400060228     C*
059500060228     C                   ENDSR
059933060412     C*-----------------------------------------------------*
059934081210     C* sraggio - aggiorno FNARB per assegnati o scrive FIINC per prepagati
059935060412     C*-----------------------------------------------------*
059936090204     c     sraggio       begsr
059937081210     c* assegnato
0602000812101    c                   if        v03tdc = 'A'
060300060301     C                   Z-ADD     v03aas        KAAS
060400060301     C                   Z-ADD     v03lnp        KLNP
060500060307     C                   Z-ADD     v03NRs        KNRS
060600060301     C                   Z-ADD     v03NSP        KNSP
060700060301     C     Kspe          CHAIN     FNARB01L
0608000604122    c                   if        %found(fnarb01l)
060900081210     c                   if        arbica =  'R' and ar6dft <> 0 and
060901080521     c                             (§3ati1 = 'A' or §3ati2 = 'A')
061000081210     c                   eval      arbica = 'Y'
061100060301     c                   end
061500030730     C                   UPDATE    FNARB000
0616000604122    c                   end
0616010902042    c                   end
062049081210     c* prepagato
062050081210     c                   if        v03tdc = 'P'
062400060307     C     Kinc          CHAIN     Fiinc01L
0625000812102    c                   if        not %found(fiinc01l)
062600081210     c                   clear                   fiincds
062700081210     c                   eval      INCaas = v03aas
062701081210     c                   eval      INClnp = v03lnp
062702081210     c                   eval      INCnrs = v03nrs
062703081210     c                   eval      INCnsp = v03nsp
062704081210     c                   eval      INCtpd = v03tdc
062705081210     c                   eval      INCTII = 'SC'
062706081210     c                   eval      INCFIV = ar6FIV
062707081210     c                   eval      INCDFT = ar6DFT
062708081210     c                   eval      INCNFT = ar6NFT
062709081210     c                   eval      INCIMP = ar6Ift
063700081210     C                   eval      INCDIV = ar6DIV
063701081210     C                   eval      INCfle = dutpou
063702081210     C                   eval      INCcuc = dutcou
063703081210     C                   eval      INCnmi = 9999999
063704081210     C                   eval      INCdmi = 99991231
063705081210     C                   eval      INCCHD = 'N'
063706081210     C                   eval      INCRNAGEN = 'N'
063707081210     C                   write     fiinc000
0638000604122    c                   end
0638010604121    c                   end
066800030730     c                   commit
066900030730     c*
067000030730     c                   endsr
067511060915     C*-----------------------------------------------------*
067512060915     C*    Operazioni iniziali
067513060915     C*-----------------------------------------------------*
067514060915     C     *INZSR        BEGSR
067515060915     C*
067600940322     C     *ENTRY        PLIST
067700940322     C                   PARM                    KPJBA
067800020905     c*
067900020905     c     *dtaara       define    §azute        azuteds
068000020905     c     *dtaara       define    §datiute      ddatiute
068100020905     C                   in(E)     *dtaara
068200020905     C                   IF        %Error  or  RSUT = *blanks
068300020905     C                   call      'TIBS34R'
068400020905     C                   parm                    Tibs34Ds
068500020905     C                   in        *dtaara
068600020905     c                   ENDIF
069500940322      *---------------------------------------------------------------*
069600940322      * CHIAVI                                                        *
069700940322      *---------------------------------------------------------------*
069800060307     C     KTAB          KLIST
069900060307     C                   KFLD                    tblKUT
070000060307     C                   KFLD                    tblCOD
070500060307     C                   Z-ADD     1             tblKUT
071700060301     C     Kspe          KLIST
071800060301     C                   KFLD                    KAAS
071900060301     C                   KFLD                    KLNP
072000060301     C                   KFLD                    KNRS
072100060301     C                   KFLD                    KNSP
072200060301     C     Kinc          KLIST
072300060301     C                   KFLD                    v03AAS
072400060301     C                   KFLD                    v03LNP
072500060301     C                   KFLD                    v03NRS
072600060301     C                   KFLD                    v03NSP
072700081210     C                   KFLD                    v03tdc
072800030730     C*
072900081210     C                   MOVEL     'CNC0W8R'     VIDPGM
073000060301     C                   MOVE      *year         V03AAS
073100060307     c*
073200060307     C                   Z-ADD     0             X                 3 0
073300060307     C                   MOVEL     '3A'          tblCOD
073400060307     C     KTAB          setll     TABEL00F
073500060307     c                   do        *hival
073600060307     C     KTAB          reade     TABEL00F
073700060307     C                   if        %eof(tabel00f)
073800060307     c                   leave
073900060307     c                   end
074000060307     C     X             iflt      100
074100060307     C     TBLFLG        andeq     ' '
074200060307     C                   ADD       1             X
074300060307     C                   MOVEL     TBLKEY        C3A(X)
074400060307     C                   MOVEL     TBLUNI        D3A(X)
074500060307     C                   END
074600060307     C                   ENDdo
074700940322     C*
074800940322     C                   ENDSR
