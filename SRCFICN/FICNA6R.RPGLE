000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020305
000500020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000600020305
000700090506     Ffiadt01l  if   e           k disk
000800081003     Ffiadd01l  if   e           k disk
000900081003     Ffiapd01l  if   e           k disk
001000060320     Fanfrn01l  if   e           k disk
001100080918     Fancdp01l  if   e           k disk
001200060321     Fansog01l  if   e           k disk
001300080916     Fanind01l  if   e           k disk
001400060320     Fazorg01l  if   e           k disk
001401170711     Ftntbe01l  if   e           k disk
001500101111     F******ansif01l  if   e           k disk
001600090911     Fficna6rp  O    e             PRINTER infds(prtds) usropn
001700090911     Fficna6rpb O    e             PRINTER usropn
001800011026
001900020304     dkpjba          e ds
002000090911     dtrul28ds       e ds
002100111213     dtrul13ds       e ds
002200160315     dtrtcm1ds       e ds
002300120314      *
002400120314     D trul33ds      E DS
002500020304
002600020304     dparam            ds
002700081003     D  parpdr                        7s 0
002800081003     D  parprg                        3s 0
002900081003     D  partip                        1
003000090427     d  pardtcert                     8  0
003100121130     d  parnrc                        7s 0
003200121130     d  parfil                        3s 0
003300121130     D  paroutq                      10
003400090427     d  parerr                        1
003500111201     d  pardfa                        8  0
003600121130     d  parsocprt                     3
003700020304
003800120322     d savkpjbu        s                   like(kpjbu)
003900090911     d cbarre15        s             15
004000090911     d cbarre16        s             16
004100020402     d modo            s              8
004200111214     d cost3d          s            120
004300111214     d cost3e          s            120
004400160503     d filpad          s              3
004500111214     d cambio          s              1
004600020305     d dataiso         s               d   datfmt(*iso)
004700121130     d dataisos        s               d   datfmt(*iso)
004800020305     d dataeur         s               d   datfmt(*eur)
004900120220      *----
005000020117     d prtds           ds
005100090911     D ODP_TYPE               81     82                                         * ODP Type
005200090911     D FILE_NAME              83     92                                         * File name
005300090911     D LIBRARY                93    102                                         * Library name
005400090911     D SPOOL_FILE            103    112                                         * Spool file name
005500090911     D SPOOL_LIB             113    122                                         * Spool file lib
005600090911     D SPOOL_NUM             123    124I 0                                      * Spool file num
005700090911     D RCD_LEN               125    126I 0                                      * Max record len
005800090911     D KEY_LEN               127    128I 0                                      * Max key len
005900090911     D MEMBER                129    138                                         * Member name
006000090911     D TYPE                  147    148I 0                                      * File type
006100090911     D ROWS                  152    153I 0                                      * Num PRT/DSP rows
006200090911     D COLUMNS               154    155I 0                                      * Num PRT/DSP cols
006300090911     D NUM_RCDS              156    159I 0                                      * Num of records
006400090911     D ACC_TYPE              160    161                                         * Access type
006500090911     D DUP_KEY               162    162                                         * Duplicate key?
006600090911     D SRC_FILE              163    163                                         * Source file?
006700090911     D VOL_OFF               184    185I 0                                      * Vol label offset
006800090911     D BLK_RCDS              186    187I 0                                      * Max rcds in blk
006900090911      *
007000090911     D OVERFLOW              188    189I 0                                      * Overflow line
007100090911      *
007200090911     D BLK_INCR              190    191I 0                                      * Blk increment
007300090911     D FLAGS1                196    196                                         * Misc flags
007400090911     D REQUESTER             197    206                                         * Requester name
007500090911     D OPEN_COUNT            207    208I 0                                      * Open count
007600090911     D BASED_MBRS            211    212I 0                                      * Num based mbrs
007700090911     D FLAGS2                213    213                                         * Misc flags
007800090911     D OPEN_ID               214    215                                         * Open identifier
007900090911     D RCDFMT_LEN            216    217I 0                                      * Max rcd fmt len
008000090911     D CCSID                 218    219I 0                                      * Database CCSID
008100090911     D FLAGS3                220    220                                         * Misc flags
008200090911     D NUM_DEVS              227    228I 0                                      * Num devs defined
008300090911      *
008400090911     D line                  367    368I 0                                      * Current line num
008500090911     D CUR_PAGE              369    372I 0                                      * Current page cnt
008600120220      *----
008700160315     D CMD             S             85    DIM(3) CTDATA PERRCD(1)              OVRPRTF
008800080916     d locemi          s             40
008900080916     D DVCOBADS      E DS
009000080916     D                                     INZ
009100080916     D CBAIBANDS     E DS
009200090114     d ficn05ds      e ds
009300080916     ddgpi           e ds
009400101111     ***************************************************************************
009500101111     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
009600101111     D ESITO_OK...
009700101111     D                 C                   0
009800101111     ***************************************************************************
009900101111     **
010000101111     ** Dichiarazione prototipi procedure esterne.
010100101111     **
010200101111     ***************************************************************************
010300101111      /DEFINE DFTACTGRP_YES
010400101111     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
010500101111      /UNDEFINE DFTACTGRP_YES
010600101111     ***************************************************************************
010700101111     **
010800101111     ** Definizione strutture dati.
010900101111     **
011000101111      **************************************************************************
011100101111     D tibsSocI0...
011200101111     D               E DS                  QUALIFIED
011300101111     D                                     INZ
011400101111     D tibsSocO0...
011500101111     D               E DS                  QUALIFIED
011600101111     D                                     INZ
011700101111     ***************************************************************************
011800101111     **
011900101111     ** Definizione variabili modulo/programma.
012000101111     **
012100101111     ***************************************************************************
012200101111     D prmRqsOpCode...
012300101111     D                 S             10A
012400101111     D prmRpyOpCode...
012500101111     D                 S             10A
012600101111     D prmRpyIdMsg...
012700101111     D                 S             10I 0
012800101111     D prmRqsFormato...
012900101111     D                 S             10A
013000101111     D prmRqsDataSize...
013100101111     D                 S             10I 0
013200101111     D prmRpyFormato...
013300101111     D                 S             10A
013400101111     D prmRpyDataSize...
013500101111     D                 S             10I 0
013600101111
013700000000     C*---------------------------------------------------------------*
013800020304      *
013900090512     c                   eval      *inlr = *on
014000020308     c
014100081003     c* imposto costanti nel caso non reperisco anagrafico
014200081003     c                   eval      costia= 'Ragione sociale ___________________+
014300081003     c                             ____________________________________________'
014400081003
014500081003     c                   eval      costib= 'Indirizzo__________________________+
014600081003     c                             ____________________________________________'
014700081003
014800081003     c                   eval      costic= 'CAP______________ Città____________+
014900081003     c                             ____________________________________________'
015000081003
015100081003     c                   eval      costif= 'Recapito __________________________+
015200081003     c                             ____________________________________________'
015300081003     c                   eval      costid= 'Partita IVA _______________________+
015400081003     c                             ____________________________________________'
015500081003     c                   eval      cost8c= '|__|__|__|__|  -
015600081003     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
015700081003     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
015800081003      * decodifico fornitore
015900081003     C                   clear                   ragsvet
016000081003     c                   movel     *all'_'       cdpdescr
016100081003     C     kfrn          chain     anfrn01l
016200081003     c                   if        %found(anfrn01l)
016300081003     C     kcdp          chain     ancdp01l
016400081003     C     frnsogg       chain     ansog01l
016500081003     c                   if        %found(ansog01l)
016600081003     C                   MOVEL     sogdes        ragsvet          44
016700081003     c                   endif
016800081003     c                   endif
016900081003     c                   eval      cost8b= %trimr(cdpdescr) +' '+
017000081003     c                             'disposto su codice IBAN:'
017100081003     c* indirizzo
017200081003     c                   clear                   tipo
017300081003     c                   clear                   codice
017400081003     C     Kind          CHAIN     anind01l
017500081003     c                   if        %found(anind01l)
017600090407     C                   eval      costia = 'Ragione sociale ' + %trim(sogdes)
017700090407     c                              + ' ' + %trim(sogdesagg)
017800081003     C                   eval      costib = 'Indirizzo ' + indindriz
017900081003     c                   if        indprov <> ' '
018000081003     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
018100081003     c                   else
018200081003     C                   eval      locemi = indlocalit
018300081003     c                   end
018400111213     c                   if        datafirma1 <> ' '
018500111215     c                   eval      cost9x=%trimr(indlocalit)+', ' +
018600111215     c                             datafirma1
018700111215     c                   else
018800111215     c                   eval      cost9x = indlocalit
018900111213     c                   end
019000081003     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
019100081003     c                             locemi
019200081003     c                   end
019300081003     C                   eval      costid = 'Partita IVA ' + sogpartiva
019400091217      **
019500091217      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
019600091217      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
019700091217      **
019800091217      **  Aggiunge il Codice Fiscale
019900091217     c                   if        sogcdfisc <> ' ' and indprov <> ' '
020000091217     c                   eval      costiD = %Trim(costiD) +
020100091217     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
020200091217     C                             ': ' + %trimr(sogcdfisc)
020300091217     c                   end
020400091217      **
020500091217     c                   if        indtelex <> ' ' and
020600091217     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
020700091217     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
020800091217     c
020900091217     c                   end
021000091217      **
021100081003     c* indirizzo recapito
021200081003     C                   SETOFF                                       89
021300081003     c                   eval      tipo ='IF'
021400081003     c                   eval      codice ='000'
021500081003     C     Kind          CHAIN     anind01l
021600081003     c                   if        %found(anind01l)
021700081003     C                   SETON                                        89
021800090407     c                   eval      costif= 'Recapito ' + %trim(sogdes)
021900090407     c                              + ' ' + %trim(sogdesagg)
022000081003     C                   eval      costiH = indindriz
022100081003     c                   if        indprov <> ' '
022200081003     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
022300081003     c                   else
022400081003     C                   eval      locemi = indlocalit
022500081003     c                   end
022600081003     C                   eval      costiI = indcap + ' ' +
022700081003     c                             locemi
022800081003     c                   end
022900081003     c* reperisco IBAN
023000081003     C                   RESET                   DVCOBADS
023100081003     C                   RESET                   CBAIBANDS
023200081003
023300090529     C                   EVAL      CoBSoc = adtcsf
023400081003     C                   EVAL      CoBCOF = 'F'
023500090529     C                   EVAL      CoBKsc = adtcdf
023600081003     C                   CALL      'DVCOBA'
023700081003     C                   PARM                    DVCOBADS
023800081003A0846C                   PARM                    CBAIBANDS
023900081003     C                   IF        CoBErr = *OFF
024000081003     C                   EVAL      cost8c = CBaStato + CbaCheck
024100081003     C                             + %TRIMR(CBaCoorI)
024200081003     c                   end
024300090911     c                   clear                   BCD128
024400090911     c                   movel     adtpdr        cbarre15
024500090911     c                   move      alladata      cbarre15
024600090911     c                   exsr      srchkdgt
024700090911     c                   movel(p)  Cbarre16      BCD128
024800081003      *stampa testata allegato
024900120314     c                   exsr      repNUM
025000090910     c                   write     testata
025100160412     c*m                 write     testadata
025200090910     c                   write     testadata1
025300090910     c                   write     testo
025400120220      **
025500120314     c                   exsr      repNUM
025600090911     c                   write     xestata
025700160412     c*m                 write     xestadata
025800090911     c                   write     xestadata1
025900090911     c                   write     xesto
026000120220      **
026100090427     c                   seton                                        66
026200081003      *
026300081003     c                   exsr      elabora
026400081003      *
026500090910     C                   write     piede
026600111214     c*                  if        prezzo <> 0
026700111214     C*                  write     piedec
026800111214     c*                  end
026900120220      **
027000090911     C                   write     xiede
027100111214     c*                  if        prezzo <> 0
027200111214     C*                  write     xiedec
027300111214     c*                  end
027400120220      **
027500090911     c                   exsr      cambiopagina
027600090910     c                   write     piede1
027700120220      *
027800090911     c                   write     xiede1
027900120220      *
028000090911     c                   exsr      cambiopagina
028100090910     c                   write     piede2
028200120220      *
028300090911     c                   write     xiede2
028400120220      *
028500090911     c                   exsr      cambiopagina
028600090910     c                   write     piede3
028700120220      *
028800090911     c                   write     xiede3
028900120220      *
029000090911     c                   exsr      cambiopagina
029100090910     c                   write     piede4
029200120220      *
029300090911     c                   write     xiede4
029400120220      *
029500090911     c                   exsr      cambiopagina
029600090910     c                   write     piede5
029700120220      *
029800090911     c                   write     xiede5
029900120220      *
030000090911     c                   exsr      cambiopagina
030100090910     c                   write     piede6
030200120220      *
030300090911     c                   write     xiede6
030400120220      *
030500090911     c                   exsr      cambiopagina
030600090910     c                   write     piede7
030700120220      *
030800090911     c                   write     xiede7
030900120220      *
031000090911     c                   exsr      cambiopagina
031001170726     c                   eval(h)   §PBA  = (Line / 9) + 0.1
031100170726     c                   write     piede8
031101170727     c                   if        pardfa < datanewf
031102170727     c                   write     piede8bv
031103170727     c                   else
031104170726     c                   write     piede8b
031105170727     c                   endif
031200120220      *
031300090911     c                   write     xiede8
031400120220      *
031500170727     c*m                 exsr      cambiopagina
031501170726     c*m                 if        pardfa < datanewf
031600170726     c*m                 write     firmaimg
031601170726     c*m                 else
031602170726     c*m                 write     firmapro
031603170726     c*m                 endif
031700120220     c                   write     piede9
031800120220      *
031900090911     c                   if        cambio = *blank
032000090910     c                   write     barcode
032100120220      **
032200160414     c*m                 write     paginablk
032300120220      **
032400160414     c*m                 write     xaginablk
032500090910     c                   endif
032600120220      **
032700020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
032800090911     C                   close     ficna6rp
032900160315     C                   close     ficna6rpb
033000160315      * richiamo routine generazione PDF
033100160315      *
033200160318     c                   exsr      generaPDF
033300160315      *
033400160315     C                   MOVEL(p)  CMD(3)        COMMAN          170
033500120220     C                   EVAL      %SUBST(comman:13:11) = 'FICNA6RP)  '
033600160315     C                   Z-ADD     170           LUNG             15 5
033700020308     C                   CALL      'QCMDEXC'
033800020308     C                   PARM                    COMMAN
033900020308     C                   PARM                    LUNG
034000020304
034100090911     C                   close     ficna6rpb
034200120220     C                   MOVEL(p)  CMD(3)        COMMAN
034300120220     C                   EVAL      %SUBST(comman:13:11) = 'FICNA6RPB) '
034400160315     C                   Z-ADD     170           LUNG
034500081007     C                   CALL      'QCMDEXC'
034600081007     C                   PARM                    COMMAN
034700081007     C                   PARM                    LUNG
034800101111      *
034900101111     ** Chiudo il programma.
035000101111     C                   CALL      'TIBSSOCR'
035100101111     C                   PARM      'FINALIZE'    prmRqsOpCode
035200101111     C                   PARM                    prmRpyOpCode
035300101111     C                   PARM                    prmRpyIdMsg
035400081007
035500020308     c                   seton                                        lr
035600020308      *--------------------------------------------------
035700020308     c     elabora       begsr
035800020308      *--------------------------------------------------
035900020411     c
036000081003     c     kadt          setll     fiadd01l
036100081003     c                   do        *hival
036200081003     c     kadt          reade     fiadd01l
036300081003      *fine file
036400081003     c                   if        %eof(fiadd01l)
036500081003     c                   leave
036600081003     c                   endif
036700090911     c                   exsr      cambiopagina
036800081003      *decodifica partenza
036900081003     c     addlnp        chain     azorg01l
037000081003     c                   if        %found(azorg01l)
037100081003     c                   movel     orgdes        partenza         20
037200081003     c                   else
037300081003     c                   clear                   partenza
037400081003     c                   endif
037500081003      *decodifica arrivo
037600081003     c     addlna        chain     azorg01l
037700081003     c                   if        %found(azorg01l)
037800081003     c                   movel     orgdes        arrivo           20
037900081003     c                   else
038000081003     c                   clear                   arrivo
038100081003     c                   endif
038200081003      * stampa riga listino attiva
038300090910     c                   write     dettaglio
038400120220      *
038500090911     c                   write     xettaglio
038600120220      *
038700081003     c                   enddo
038800060320
038900020308     c                   endsr
039000020304
039100090911     C*-----------------------------------------------------*
039200090911     C* write per cambio pagina
039300090911     C*-----------------------------------------------------*
039400090911     C     cambiopagina  BEGSR
039500120220      *
039600120220      *   test della linea portato dentro la routine:
039700120221     c                   if        line > 92
039800120220      *
039900090911     c                   write     barcode
040000120220      *
040100120314     c                   exsr      repNUM
040101170711     c                   if        pardfa < datanewf
040200090911     c                   write     firmaimg
040203170621     c                   else
040204170621     c                   write     firmapro
040205170621     c                   endif
040300090911     c                   write     piede9
040400090911     c                   write     testata
040500120220      *
040600120314     c                   exsr      repNUM
040700120220     c                   write     barcode
040800090911     c                   write     xestata
040900120220      *
041000090911     c                   move      'X'           cambio
041100120220      *
041200120220     c                   endIF
041300120220      *
041400090911     c                   endsr
041500090911     C*-----------------------------------------------------*
041600090911     C* controllo check digits
041700090911     C*-----------------------------------------------------*
041800090911     C     srchkdgt      BEGSR
041900090911      *
042000090911     c                   movel(p)  cbarre15      cbarre16         16
042100090911     C                   clear                   trul28ds
042200090911     c                   eval      i28mod = 'BAR'
042300090911     c                   eval      i28cod = cbarre16
042400090911     c                   call      'TRUL28R1'
042500090911     c                   parm                    trul28ds
042600090911     c* se reperito barcode imposto i campi in stampa
042700090911     c                   if        o28err = *blanks
042800090911     c                   move      O28CKD        cbarre16
042900090911     c                   end
043000090911      *
043100090911     C                   ENDSR
043200011026     c*-------------------------------------------------
043300011026     c     *inzsr        begsr
043400011026     c*-------------------------------------------------
043500020304     c     *entry        plist
043600020304     c                   parm                    kpjba
043700020304     c                   movel     kpjbu         param
043800020304
043900120217     c                   clear                   di_PEDINI         1
044000120221     c                   setoFF                                       88
044100120606     c                   if        parnrc > 9000000
044200120217     c                   eval        di_PEDINI = 'S'
044300120221     c                   setoN                                        88
044400120217     c                   end
044500120217
044600090427     c                   move      pardtcert     dataiso
044700090427     c                   move      dataiso       dataeur
044800090427     c                   move      dataeur       datacerta         8 0
044900111205     c                   if        pardfa <> 0
045000111201     c                   move      pardfa        dataiso
045100111201     c                   move      dataiso       dataeur
045200111201     c                   move      dataeur       datafirma        10
045300111213     c* la data firma + 1
045400111213     C                   clear                   trul13ds
045500111213     c                   eval      i13fil       = parfil
045600111213     C                   move      dataiso       i13din
045700111213     C                   EVAL      i13gio = 1
045800111213     C                   CALL      'TRUL13R'
045900111213     C                   PARM                    trul13ds
046000111213     C                   IF        o13err <> 'E'
046100111213     c                   move      o13dfi        dataiso
046200111213     c                   move      dataiso       dataeur
046300111213     c                   move      dataeur       datafirma1       10
046400111213     c                   else
046500111213     c                   eval      datafirma1 = datafirma
046600111213     c                   end
046700111213     c                   else
046800111213     c                   clear                   datafirma
046900111213     c                   clear                   datafirma1
047000111213     c                   end
047100101111     C*
047200101111     ** Inizializzo il programma.
047300101111     C                   CALL      'TIBSSOCR'
047400101111     C                   PARM      'INIT'        prmRqsOpCode
047500101111     C                   PARM                    prmRpyOpCode
047600101111     C                   PARM                    prmRpyIdMsg
047700090427
047800081003     c     kapd          klist
047900081003     c                   kfld                    tipoa             1
048000081003     c                   kfld                    parpdr
048100081003     c                   move      'D'           tipoa
048200081003     c     kadt          klist
048300081003     c                   kfld                    parpdr
048400081003     c                   kfld                    parprg
048401170711     c     ktbe          klist
048402170711     c                   kfld                    tbecod
048403170711     c                   kfld                    tbeke1
048500081003     c
048600060320     c     kfrn          klist
048700090529     c                   kfld                    adtcsf
048800090529     c                   kfld                    adtcdf
048900080918     c     kcdp          klist
049000090529     c                   kfld                    adtcsf
049100080918     c                   kfld                    frncondpag
049200080916     c     kind          klist
049300080916     c                   kfld                    frnsogg
049400080916     c                   kfld                    tipo              2
049500080916     c                   kfld                    codice            3
049600021203
049700020218     C                   time                    w0120            14 0
049800020218     C                   move      w0120         wdat              8 0
049900090114      * data da impostare su data stampa e convalida se lancio = 'C'
050000020308     c                   move      wdat          dataeur
050100020308     c                   move      dataeur       dataiso
050200020308     c                   move      dataiso       dataoggi          8 0
050201170711      * tabella 007 per data firma documento
050202170711     c                   eval      tbecod = '007'
050203170711     c                   eval      tbeke1 = '*FIRPRONI'
050204170711     c                   eval      datanewF = 20391231
050205170711     c     ktbe          chain     tntbe01l
050206170711     c                   if        %found(tntbe01l)
050207170711     c                   movel     tbeke2        dataiso
050208170711     c                   movel     dataiso       datanewf          8 0
050209170711     c                   endif
050300081003     c* aggancio testata
050400090406     c                   clear                   tmp_caric         4 0
050500090406     c                   clear                   tmp_scar          4 0
050600081003     c     kadt          chain     fiadt01l
050700081003     c                   if        %found(fiadt01l)
050800090512     c                   if        adtatb <> *blank
050900090512     c                   move      '1'           parerr
051000090512     c                   movel     param         kpjbu
051100090512     c                   return
051200090512     c                   endif
051300090406     c                   z-add     adtTMPC       tmp_caric
051400090406     c                   z-add     adtTMPS       tmp_scar
051500090529     c                   move      adtcdf        adtcdfs           7
051600090512     c                   else
051700090512     c                   move      '2'           parerr
051800090512     c                   movel     param         kpjbu
051900090512     c                   return
052000081003     c                   endif
052100081003      * data decorrenza da stampare
052200081003     c                   move      adtddt        dataiso
052300081003     c                   move      dataiso       dataeur
052400081003     c                   move      dataeur       alladata          8 0
052500081003     c* aggancio anagrafica autisti per reperire codice fornitore
052600081003     c     kapd          chain     fiapd01l
052700081003     c                   if        %found(fiapd01l)
052800090512     c                   if        apdatb <> *blank
052900090512     c                   move      '3'           parerr
053000090512     c                   movel     param         kpjbu
053100090512     c                   return
053200090512     c                   endif
053300081003     c                   endif
053400011026
053500020117     c                   z-add     99            line
053600020305     c                   move      *all'_'       separa          100
053700060321      * ragione sociale società mittente
053800101111     c**** adtcsf        chain     ansif01l
053900101111     c*********          if        %found(ansif01l)
054000101111     c*********          movel     sifrgs        ragsmit          30
054100101111     c*********          endif
054200101111      **
054300120221     c                   clear                   ragsmit
054400111215     c                   if        datafirma = ' '
054500111215     c                   eval      cost9 = 'Luogo  _______________________'
054600111215     c                   eval      cost91= 'Data   _______________________'
054700111215     c                   end
054800101111     c                   exsr      decod_societa
054900101111      **
055000080916     c* prezzo iniziale carburante legge 133
055100111205     c                   if        adtpcar <> 0
055200111205     c                   eval      prezzo = adtpcar
055300111205     c                   else
055400090114     c                   clear                   ficn05ds
055500090114     c                   call      'FICN05R'
055600090114     c                   parm                    ficn05ds
055700111205      ** se non ha agganciato la tabella imposta fisso il prezzo
055800111205     c                   if        o05err = 'E'
055900111205     c                   eval      prezzo = 1,518
056000111205     c                   else
056100111205      **  altrimenti lo prende dalla tabella
056200111205     c                   movel     o05gpi        dgpi
056300111205     c                   z-add     D§GPIGPI      prezzo            5 3
056400111205     c                   end
056500111205     c                   end
056600020304
056700090114     c                   eval      costt = 'ALLEGATO TARIFFARIO IN VIGORE DAL'
056800020412     c
056900080916     c* imposto costanti nel caso non reperisco anagrafico
057000080916     c                   eval      costia= 'Ragione sociale ___________________+
057100080916     c                             ____________________________________________'
057200080916
057300080916     c                   eval      costib= 'Indirizzo__________________________+
057400080916     c                             ____________________________________________'
057500080916
057600080916     c                   eval      costic= 'CAP______________ Città____________+
057700080916     c                             ____________________________________________'
057800080916
057900080916     c                   eval      costif= 'Recapito __________________________+
058000080916     c                             ____________________________________________'
058100080916     c                   eval      costid= 'Partita IVA _______________________+
058200080916     c                             ____________________________________________'
058300080916     c                   eval      cost8c= '|__|__|__|__|  -
058400080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
058500080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
058600020411     c*costanti pretariffa
058700020411     c                   eval      costa = 'Allegato tariffario al contratto +
058800060321     c                             di trasporto sottoscritto tra la società '
058900060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
059000060321     c                   eval      costa2b= 'codice fornitore'
059100081212     c                   eval      costa3= '(di seguito denominata VETTORE) -
059200120221     c                             e relativo alle prestazioni dell'' -
059300081212     c                             autotrasportatore'
059400081212     c
059500120217     c                   if        di_Pedini = *blank
059600060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
059700081003     c                             12 del suddetto contratto di trasporto, con+
059800111201     c                             cordano i corrispettivi per i trasporti '
059900120217     c                   elseif    di_Pedini = 'S'
060000120217     c                   eval      cost1 = 'Le parti come convenuto all''art.+
060100120217     c                             10 del suddetto contratto di trasporto, con+
060200120217     c                             cordano i corrispettivi per i trasporti '
060300120217     c                   end
060400120217      ***
060500111201     c                   eval      cost2 = 'comprensivi delle eventuali operazi-
060600111201     c                             oni connesse al trasporto stesso, nonchè di +
060700111202     c                             qualsiasi onere necessario per '
060800111202     c                   eval      costb = 'l''esecuzione +
060900111202     c                             del trasporto (pedaggi, diritti di +
061000111202     c                             transito, equipaggiamento del veicolo, temp+
061100111202     c                             i di carico e scarico,'
061200111202     c                   eval      costc = 'ecc.) secondo la seguente tariffa:'
061300020306
061400020411     c*costanti piede
061500020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
061600020411     c                             he i corrispettivi indicati nelle sopraccita+
061700020411     c                             te tariffe possano essere oggetto di'
061800020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
061900020411     c                             one, a seguito di modifiche pattuite tra le +
062000020412     c                             parti del progetto operativo'
062100020412     c                   eval      cost3c= 'originariamente concordato.'
062200080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
062300080916     c                             che i corrispettivi di cui al presente alleg-
062400080916     c                             ato sono stati concordati'
062500080916     c                   eval      cost3e= 'con il mittente sulla base del -
062600090911     c                             costo del carburante pari a euro'
062700090403
062800090403     c                   eval      cost3_1 = 'Ai sensi e per gli effetti dell''+
062900111205     c                             art.6, comma 3 lett. e-bis D.Lgs. 286/05,il-
063000090403     c                              contratto di trasporto di cui il presente'
063100090403     c                   eval      cost3_2 = 'costituisce allegato, deve intend+
063200090403     c                             ersi integrato con l''indicazione dei seguen+
063300090417     c                             ti tempi massimi di carico e scarico che'
063400090417     c                   eval      cost3_3 = 'le stesse parti indicano in compl+
063500111202     c                             essive ore 2 (due) in relazione a ciascun +
063600111202     c                             trasporto da effettuarsi '
063700090417     c                   eval      cost3_4 = 'nell''arco della giornata.'
063800080916
063900020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
064000020402     c                              società da questi delegata ad emetter+
064100020411     c                              e alla fine di ogni mese in nome e per '
064200020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
064300111201     c                             a relativa ai trasporti ed alle operazioni +
064400111212     c                             connesse effettuati in tale periodo.'
064500090417
064600111202     c                   eval      cost5b= 'Tale fattura com-
064700060526     c                             prenderà anche i corrispettivi per le presta+
064800060526     c                             zioni di trasporto non previste'
064900020306
065000081003     c                   eval      cost5c= 'dal presente allegato  , alle quali-
065100060526     c                              le parti in ogni caso riconoscono la piena +
065200060526     c                             validità ed efficacia del '
065300060526
065400120217     c                   if        di_Pedini = *blank
065500081212     c                   eval      cost5d= 'contenuto degli articoli 12 e 13 -
065600060526     c                             del contratto di trasporto.'
065700120217     c                   elseIf    di_Pedini = 'S'
065800120217     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
065900120217     c                             del contratto di trasporto.'
066000120217     c                   end
066100060526
066200020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
066300111202     c                             dei fogli viaggio che contengono in dettagl+
066400111202     c                             io i trasporti e le operazioni'
066500020411
066600111212     c                   eval      cost6a= 'connesse effettuati.'
066700090417     c                   eval      cost6b= 'La fattura sarà emessa con Iva +
066800090417     c                             ad esigibilità immediata.'
066900020411
067000020411     c                   eval      costi = 'L''intestazione del vettore da rip+
067100020411     c                             ortare sulla fattura emessa è:'
067200020411
067300020411     c                   eval      costie= 'Il vettore in caso di variazione de+
067400020412     c                             ll''intestazione ne darà immediata comunic+
067500020412     c                             azione al mittente.'
067600020411
067700090417     c                   eval      cost7 = 'Il presente allegato decorre d+
067800090417     c                             alla data del __________, e costituisce +
067900090417     c                             parte integrante del contratto di '
068000090417     c                   eval      cost71 = 'trasporto suddetto.'
068100090417
068200081003     c*                  eval      cost7a= 'scritto relativo al corrispettivo +
068300081003     c*                            per l''esecuzione di servizi di trasporto +
068400081003     c*                            e costituisce parte integrante del '
068500081003     c*                  eval      cost7b= 'contratto di trasporto suddetto.'
068600090417      *******
068700120217     c                   if        di_Pedini = *blank
068800020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
068900020412     c                             la piena validità ed efficacia del contenut+
069000081003     c                             o degli articoli 12 e 13 del'
069100120217     c                   elseIf    di_Pedini = 'S'
069200120217     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
069300120217     c                             la piena validità ed efficacia del contenut+
069400120217     c                             o degli articoli 10 e 14 del'
069500120217     c                   end
069600020412     c                   eval      cost7d='contratto di trasporto.'
069700090417      *****
069800020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
069900020411     c                             pagamento dei corrispettivi fatturati verrà +
070000090417     c                             effettuato dalla mittente a 40 giorni'
070100111212     c                   eval      cost81= 'mediante bonifico bancario disposto-
070200090417     c                              su codice IBAN:'
070300020402
070400020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
070500020411     c                             lle coordinate bancarie ne darà +
070600020412     c                             immediata comunicazione al mittente.'
070700090417
070800090417     c                   eval      cost8f1= 'Il vettore approva espressamente +
070900090417     c                             il contenuto del presente allegato ed in +
071000090417     c                             particolare le pattuizioni relative '
071100090417
071200090417     c                   eval      cost8f2= 'alla quantificazione, modalità e +
071300090417     c                             termini di pagamento dei corrispettivi.'
071400090417
071500081003     c                   eval      cost8g= 'A decorrere dalla data odierna, +
071600081003     c                             il presente allegato annulla e sostituisce +
071700081003     c                             ogni precedente accordo '
071800081003     c                   eval      cost8h= 'scritto o verbale relativo al +
071900081003     c                             corrispettivo.'
072000111215     c                   if        datafirma = ' '
072100111215     c                   eval      cost9x = 'Luogo  _______________________'
072200111215     c                   eval      cost91x= 'Data   _______________________'
072300111215     c                   end
072400090512     c                   eval      cost10= 'Firma del Mittente ________________+
072500060322     c                             _________________________'
072600090512     c                   eval      cost11= 'Firma del Vettore _________________+
072700060322     c                             _________________________'
072800090911     c*                  eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
072900090911     c*                             VALIDA CON I REFERIMENTI RICHIESTI'
073000020313
073100090420     c***********        eval      costVe= '(Copia per il Vettore) '
073200090420     c***********        eval      costMi= '(Copia per il Mittente)'
073300160315      *
073400160315      ** preparazione dati per invio a poste come PDF
073500120220      *
073600090512     c* forzatura coda se non impostata a video
073700090512     c                   if        paroutq = *blank
073800090512     c                   eval      paroutq = 'P046B052AU'
073900090512     c                   endif
074000120220      *
074100090512     c* forzatura coda se non impostata a video
074200020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
074300120220      *----------
074400160315      * con la firma
074500160315     c                   clear                   primo             1
074600160315     c                   exsr      inviaPDF
074700160315      *----------
074800120220     C                   MOVEL(p)  CMD(1)        COMMAN
074900120220     C                   EVAL      %SUBST(comman:14:11) = 'FICNA6RP)  '
075000120220     C                   EVAL      %SUBST(comman:38:11) = 'FICNA6RP)  '
075100120220     C                   MOVE      CMD(2)        COMMAN
075200160315     c***                if        paroutq <> *blank
075300160315     c***                EVAL      %SUBST(comman:145:16) =
075400160315     c***                          'OUTQ(' + paroutq + ')'
075500160315     c***                endif
075600160315      *
075700160315     c                   eval      commanl = %trim(Comman)+ trtcm1ds + ''')'
075800160315     c                   eval      lung = %len(%trim(commanl))
075900120220      *
076000160315     C****               Z-ADD     160           LUNG
076100020308     C                   CALL      'QCMDEXC'
076200160315     C                   PARM                    COMMANl         500
076300020308     C                   PARM                    LUNG
076400090911     C                   OPEN      ficna6rp
076500160315      * con il Barcode
076600160315     c                   move      'X'           primo
076700160315     c                   exsr      inviaPDF
076800120220      *----------
076900120220     C                   MOVEL(p)  CMD(1)        COMMAN
077000120220     C                   EVAL      %SUBST(comman:14:11) = 'FICNA6RPB) '
077100120220     C                   EVAL      %SUBST(comman:38:11) = 'FICNA6RPB) '
077200120220     C                   MOVE      CMD(2)        COMMAN
077300120220      *
077400160315     c****               if        paroutq <> *blank
077500160315     C****               EVAL      %SUBST(comman:145:16) =
077600160315     c****                         'OUTQ(' + paroutq + ')'
077700160315     c****               endif
077800120220      *
077900160315     c                   eval      commanl = %trim(Comman)+ trtcm1ds + ''')'
078000160315     c                   eval      lung = %len(%trim(commanl))
078100160315     C***                Z-ADD     160           LUNG
078200081007     C                   CALL      'QCMDEXC'
078300160315     C                   PARM                    COMMANl
078400081007     C                   PARM                    LUNG
078500090911     C                   OPEN      ficna6rpb
078600120220      *----------
078700120220      *
078800011026     c                   endsr
078900160315     C*------------------------------------------------------*
079000160315     C*  reperisce dati necessari preparazione PDF
079100160315     C*------------------------------------------------------*
079200160315     C     inviaPDF      begsr
079300160315      * verifico se l'invio è tramite PDF
079400160315     c                   movel(p)  'EMAILIN'     paroutq
079500160315     c                   clear                   trtcm1ds
079600160315     c                   move      046           §cm1po
079700160315     c                   move      '0'           §cm1sts
079800160315     c                   eval      §cm1idp = '*'
079900160315     c                   eval      §cm1pcl = 'S'
080000160315     c                   if        primo = *blank
080100160503     c                   movel     adtpdr        filpad
080200160315     c                   eval      §cm1tips= 'AAC'
080300160315     c                   eval      §cm1var = '*FOUT*' + adtcsf + '_'
080400160503     c                             +  filpad + '_'
080500160329     c                             +  %editc(parnrc:'X') + '_'
080600160329     c                             +  %editc(adtpdr:'X') + '_'
080700160420     c                             +  %editc(adtddt:'X')
080800160315     C* Imposto i parametri x il primo pdf con firma
080900160315     C                   EVAL      pin_tla    = *blanks
081000160315     C                   EVAL      pin_tips   = §cm1tips
081100160315     C                   EVAL      pin_NomeFile = %subst(§cm1var:7)
081200160315     C                   EVAL      pin_Extfile  = '.PDF'
081300160315     C                   EVAL      pOut_esito  = ' '
081400160315     c                   else
081500160503     c                   movel     adtpdr        filpad
081600160315     c                   eval      §cm1tips = 'AAS'
081700160315     c                   eval      §cm1var = '*FOUT*' + adtcsf + '_'
081800160503     c                             +  filpad + '_'
081900160329     c                             +  %editc(parnrc:'X') + '_'
082000160329     c                             +  %editc(adtpdr:'X') + '_'
082100160420     c                             +  %editc(adtddt:'X')
082200160315     C                   EVAL      pin_tla2   = *blanks
082300160315     C                   EVAL      pin_tips2  = §cm1tips
082400160315     C                   EVAL      pin_NomeFile2 = %subst(§cm1var:7)
082500160315     C                   EVAL      pin_Extfile2  = '.PDF'
082600160315     C                   EVAL      pOut_esito2  = ' '
082700160315     c                   endif
082800160315     C*
082900160315     C                   endsr
083000160315     c*______________________________________________________________
083100160315     c     GeneraPDF     begsr
083200160315     c*______________________________________________________________
083300160315     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
083400160315     C*
083500160315     C                   CALL      'TIS7P2SR4'
083600160315     C                   PARM                    pin_tla           1
083700160315     C                   PARM                    pin_tips          3
083800160315     C                   PARM                    pin_NomeFile     60
083900160315     C                   PARM                    pin_Extfile      11
084000160315     C                   PARM                    pOut_esito        1
084100160315     C*
084200160315     C                   CALL      'TIS7P2SR4'
084300160315     C                   PARM                    pin_tla2          1
084400160315     C                   PARM                    pin_tips2         3
084500160315     C                   PARM                    pin_NomeFile2    60
084600160315     C                   PARM                    pin_Extfile2     11
084700160315     C                   PARM                    pOut_esito2       1
084800160315      *non trovato indirizzo mail destinatario invio la mail allo stesso
084900160315      * utente che sta inviando
085000160315     c
085100160315     C                   endsr
085200101111     C**************************************************************************
085300101111     C* decodifica la società
085400101111     C************************************************************
085500101111     C     Decod_societa begSR
085600101111      *
085700101111     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
085800101111     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
085900101111     C********           RESET                   tibsSocI0
086000121130     C                   EVAL      tibsSocI0.idSocieta = ParSocPrt
086100121130     c                   move      pardfa        dataisos
086200121130     C                   EVAL      tibsSocI0.dtvalidita = dataisos
086300101111     C
086400101111     C                   CALL      'TIBSSOCR'
086500101111     C                   PARM      'GETANAGRAF'  prmRqsOpCode
086600101111     C                   PARM      *BLANK        prmRpyOpCode
086700101111     C                   PARM      *ZERO         prmRpyIdMsg
086800101111     C                   PARM      'TIBSSOCI0'   prmRqsFormato
086900101111     C                   PARM                    tibsSocI0
087000101111     C                   PARM                    prmRqsDataSize
087100101111     C                   PARM      'TIBSSOCO0'   prmRpyFormato
087200101111     C                   PARM                    tibsSocO0
087300101111     C                   PARM                    prmRpyDataSize
087400101111     c                   if         PRMRPYIDMSG >= 0
087500101111     c                              and TIBSSOCO0.IDSOCIETA <> *blank
087600101111     c                   eval       ragsmit  = tibsSocO0.RAGSOCIALE
087700111213     c                   if        datafirma <> ' '
087800111215     c                   eval      cost9 = tibsSocO0.SEDlegLOC +
087900111215     c                             ', ' +  datafirma
088000111213     c* per stampa con barcode: metto anche data e luogo copia con firma
088100111213     c                   eval      cost12= tibsSocO0.SEDlegLOC+', '
088200111213     c                             + datafirma
088300111213     c                   else
088400111215     c                   eval       cost9  = tibsSocO0.SEDlegLOC
088500111213     c                   clear                   cost12
088600111213     c                   end
088700111215     c                   end
088800101111      *
088900101111     C                   ENDSR
089000011129     C*--------------------------------------------------------------------
089100120314     C/EJECT
089200120314     c**********************************************************************
089300120314     c* reperimento numero progressivo pagina
089400120314     c**********************************************************************
089500120314     C     repnum        BEGSR
089600120322     c                   movel     kpjbu         savkpjbu
089700120315     c                   clear                   numPROGR
089800120314     c                   clear                   trul33ds
089900120314     c                   move      ' '           i33tla
090000120314     c                   z-add     80            i33cnu
090100120314     c                   z-add     1             i33num
090200120314     c                   movel     trul33ds      kpjbu
090300120314     c                   call      'TRUL33R'
090400120314     c                   PARM                    kpjba
090500120314     c                   movel     kpjbu         trul33ds
090600120314     c                   if        o33err <> *zeros
090700120314     c                   dump
090800120314     c                   else
090900120314     c                   z-add     o33nrf        numPROGR
091000120314     c                   endif
091100120322     c                   movel     savkpjbu      kpjbu
091200120314     C                   ENDSR
091300120314     C************************************************************
091400090911**
091500160315OVRPRTF FILE(           TOFILE(*LIBL/           LPI(9) CPI(15) PAGRTT(0) SAVE(*YES)         1
091600160315OUTPTY(3) FORMTYPE(DOCAUTIST) PAGESIZE(115 198) OVRFLW(101) OUTQ(EMAILIN) USRDFNDTA('       2
091700120220DLTOVR FILE(                                                                                3
