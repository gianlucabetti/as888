000100990910      /TITLE Upload via Internet: decisore.
000200990908     H dftactgrp(*yes)
000300990908
000400080522     Fazorg01l  if   e           k disk
000500990908     Ftivlr00f  uf   e             disk
000600990920     Ftivtf01l  if   e           k disk
000700000623     Ftivir02l  if   e           k disk
000800080526     Ftivire1l  if   e           k disk
000900000711     Ftivss01l  if   e           k disk
001000000711     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001100000627     F                                     PREFIX(f_)
001200000711     F                                     USROPN
001300990908
001400990910     D dscmz         e ds                  inz
001500080522     D og143         e ds                  inz
001600080526     D dvire01       e ds                  inz
001700080527     D dsvlr3        e ds                  inz
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900990920     D tivlrnrrp       s             10s 0
002000021119     D sv_tivlrds    e ds                  extname(tivlr00f) prefix(sv_)
002100030617     D sv2_tivlrds   e ds                  extname(tivlr00f) prefix(sv2_)
002200990921     D prmtivlr        s                   like(tivlrds)
002300990910     D prmesito        s              1
002400000724     D prmlit          s             10    inz(*blanks)
002500000710     D prmfir          s             10    inz(*blanks)
002600021118     D prmfid          s             10    inz(*blanks)
002700051013     D prmmbrfit       s              1    inz(*blanks)
002800080526     D prmtotrcd       s              7  0 inz(*zeros)
002900990910     D wrkesito        s              1
003000000626     D dataiso         s               d
003100000626     D dataiso1        s               d
003200000626     D data8           s              8  0
003300000626     D ora             s              6  0
003400000626     D flgvirfound     s              1    inz('0')
003500000703     D wrkprg          s              8  0 inz(*zeros)
003600000703     D dwlprg          s             10    inz(*all'0')
003700030616     D savMSG          s             30
003800080522     D skLOG           s              3    DIM(200)
003900080522     D jLOG            s              3I 0
004000020107
004100020107     D*------------------
004200020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300020107     D*------------------
004400020107     D wlbda8          DS                  INZ
004500020107     D  g08dat                 1      8  0
004600020107     D  g08inv                 9     16  0
004700020107     D  g08err                17     17
004800020107     D  g08tgi                18     22  0
004900020107     D*
005000000626
005100000626
005200990908     C* Leggo il record di log.
005300990908     C                   dou       %status = 0
005400990908     C     tivlrnrrp     chain(e)  tivlr00f
005500990908     C                   enddo
005600030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
005700030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
005800030613     C                   if        %len(%trim(vlrmsg)) > 0
005900090116     C                   if        %subst(vlrfl3:1:1) = '1'
006000090116     C                   eval      savMSG = %subst(vlrmsg:51)
006100090116     C                   else
006200080522     C                   eval      savMSG = vlrmsg
006300090116     C                   endif
006400030613     C                   endif
006500000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
006600000630     C                   exsr      inzvar
006700000711     C                   exsr      readtivir
006800021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
006900021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
007000021115     C                   eval      vlrpoi = virpoi
007100021115     C                   eval      vlrfl1 = virfi2
007200021115     C                   endif
007300010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
007400010710     C* ... o ancora da data-entry giudato via Internet)
007500000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
007600000703     C                             vlrfis = *blanks or
007700011127     C                             vlrfou = *blanks or
007800011127     C                             vlrpgt = *blanks
007900000711     C* Completo i campi mancanti nel record di log
008000000711     C                   eval      vlrlis = '*LIBL'
008100000711     C                   eval      vlrfis = virfis
008200011127     C                   eval      vlrfou = virfit
008300000711     C                   eval      vlrpgt = virpgt
008400000711     C                   eval      vlrppt = virppt
008500000626     C                   else
008600000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
008700010524     C                   if        vlrprg = *blanks
008800000627     C                   exsr      calprog
008900000627     C                   endif
009000000626     C                   clear                   wrkesito
009100030616     C***                clear                   vlrmsg
009200030616     C                   move      savMSG        vlrmsg
009300021119     C*
009400021119     C* Prima della traduzione disalloco il record
009500021119     C                   unlock    tivlr00f
009600021119     C*
009700990910     C* Traduzione.
009800990910     C                   exsr      traduci
009900021119     C*
010000030617     C* Subito dopo la traduzione salvo buffer di ritorno dalla traduzione che può essere cambiato
010100030617     C                   eval      sv2_tivlrds = prmtivlr
010200030617     C* Dopo la traduzione salvo anche il buffer originale del record e rileggo lo stesso record
010300030617     C                   eval      sv_tivlrds = tivlrds
010400021119     C                   dou       %status = 0
010500021119     C     tivlrnrrp     chain(e)  tivlr00f
010600021119     C                   enddo
010700021119     C*
010800030617     C* Dopo aver letto memorizzo gli unici campi che nella traduzione possono essere cambiati
010900030617     C* (vlrfl3 = flag operativi e vlrpoi = P.O. invio file tradotto)
011000030617     C* e re-imposto il buffer salvato prima della rilettura del record.
011100021119     C                   eval      sv_vlrfl3 = vlrfl3
011200021119     C                   eval      tivlrds = sv_tivlrds
011300030617     C                   eval      vlrpoi = sv2_vlrpoi
011400021119     C*
011500000626     C* Imposto stato record.
011600000626     C                   select
011700000626     C                   when      wrkesito = ' '
011800000626     C                   eval      vlrflg = '0'
011900000626     C                   when      wrkesito = '0'
012000000626     C                   eval      vlrflg = '1'
012100000626     C                   other
012200000626     C                   eval      vlrflg = '2'
012300000626     C                   endsl
012400010615     C*
012500000626     C                   endif
012600021115     C                   exsr      updtivlr
012700000626     C*
012800000627     C                   seton                                        lr
012900000626
013000000627
013100000626
013200000627      /TITLE Valorizzazione Progressivo Applicazione
013300000627     C     calprog       begsr
013400000627     C*
013500000711     C                   open(e)   tis7prgf
013600000711     C                   if        not %error
013700000627     C                   read      tis7prgf
013800000627     C*
013900000627     C                   eval      dwlprg = f_tis7prgf
014000000627     C                   move(p)   dwlprg        wrkprg
014100000627     C                   add       1             wrkprg
014200000627     C                   move(p)   wrkprg        dwlprg
014300000627     C                   movel     vlrisv        dwlprg
014400000627     C*
014500000627     C                   eval      f_tis7prgf = dwlprg
014600010524     C                   update(e) tis7prg0
014700010524     C                   if        not %error
014800010524     C                   eval      vlrprg = dwlprg
014900000711     C                   endif
015000010524     C                   endif
015100000711     C                   close(e)  tis7prgf
015200000627     C*
015300000627     C                   endsr
015400000627
015500000627
015600000626
015700000626      /TITLE Aggiornamento TIVLR00F.
015800000626     C     updtivlr      begsr
015900000630     C*
016000000626     C* Aggiorno record di log con l'esito dell'elaborazione.
016100000627     C*                  if        vlrflg <> '0'
016200000630     C*
016300051108     C* Nel percorso nome file ricevuto inserisco anche la data/ora d effettiva elaborazione
016400051108     C* (orribilis ma mi serve e qui posso metterlo senza dare fastidio a nulla)
016500051108     C                   exsr      momentoElab
016600051108     C                   move      dataoraElab   vlrpth
016700051108     C* Quindi aggiorno il record d log corrente
016800990910     C                   update    tivlr000
016900000627     C*                  else
017000000627     C*                  unlock    tivlr00f
017100000627     C*                  endif
017200000626     C*
017300000626     C                   endsr
017400000626
017500000626
017600000623
017700000623      /TITLE Lettura TIVIR00F.
017800000626     C     readtivir     begsr
017900000623     C*
018000000626     C                   clear                   wrkesito
018100000626     C*
018200000626     C                   eval      vsssun = vlrsun
018300020107     C                   eval      vssisv = vlrisv
018400000623     C     k02vss01      chain     tivss01l
018500000623     C*
018600020107     C                   if        not %found or
018700020107     C                             vssdde > datcor or
018800020107     C                             vssdsc < datcor
018900000630     C                   eval      wrkesito='1'
019000000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
019100000630     C                                      'al servizio.'
019200030613     C                   move      savMSG        vlrmsg
019300010615     C                   exsr      sndeml
019400000626     C                   else
019500010710     C*
019600010710     C* Costruisco la parte fissa della chiave
019700010710     C                   eval      virksc = vssksu
019800010710     C                   eval      virtip = vlrtip
019900010710     C                   move      dataiso       dataiso1
020000010710     C                   adddur    1:*d          dataiso1
020100010710     C     *iso          move      dataiso1      virdti
020200010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
020300010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
020400010710     C                   if        vlrtia = 'I'
020500010710     C                   eval      flgvirfound = *on
020600010710     C                   else
020700080522     C                   eval      flgvirfound = *off
020800080522     C                   endif
020900080522     C*
021000000623     C     k03vir02      setll     tivir02l
021100000623     C     k02vir02      readpe    tivir02l
021200000623     C* Se ho letto il record
021300000623     C* ed è valido segnalo che l'ho trovato.
021400000623     C                   if        not %eof
021500000623     C                             and data8 <= virdtf
021600000623     C                   eval      flgvirfound = *on
021700000630     C                   else
021800000630     C                   if        flgvirfound = *off
021900000630     C                   eval      wrkesito='1'
022000000630     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
022100000630     C                                      ' per il cliente.'
022200030613     C                   move      savMSG        vlrmsg
022300010615     C                   exsr      sndeml
022400000623     C                   endif
022500000630     C                   endif
022600010626     C*
022700010626     C* In ogni caso =>
022800010626     C* ... reperisco regole standard.
022900000623     C                   if        flgvirfound = '1'
023000000623     C*
023100010726     C                   eval      vtftip = virtip
023200000623     C     k01vtf01      chain     tivtf01l
023300000623     C*
023400000623     C                   if        %found
023500000623     C*
023600000623     C                   if        virctl = *blanks
023700000623     C                   eval      virctl = vtfctl
023800000623     C                   endif
023900000623     C*
024000000623     C                   if        virpgt = *blanks
024100000623     C                   eval      virpgt = vtfpgt
024200000623     C                   endif
024300000725     C*
024400000725     C                   if        virppt = *blanks
024500000725     C                   eval      virppt = vtfppt
024600000725     C                   endif
024700000713     C*
024800000713     C                   if        virpgi = *blanks
024900000713     C                   eval      virpgi = vtfpgi
025000000713     C                   endif
025100000725     C*
025200000725     C                   if        virppi = *blanks
025300000725     C                   eval      virppi = vtfppi
025400000725     C                   endif
025500000623     C*
025600000623     C                   if        virfid = *blanks
025700000623     C                   eval      virfid = vtffid
025800000623     C                   endif
025900000623     C*
026000000623     C                   if        virfis = *blanks
026100000623     C                   eval      virfis = vtffis
026200000623     C                   endif
026300000623     C*
026400000623     C                   if        virfit = *blanks
026500000623     C                   eval      virfit = vtffit
026600000623     C                   endif
026700051013     C*
026800051013     C                   eval      prmmbrfit = %subst(vtffi2:1:1)
026900000623     C*
027000000630     C                   else
027100000630     C                   eval      wrkesito='1'
027200000630     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
027300030613     C                   move      savMSG        vlrmsg
027400010615     C                   exsr      sndeml
027500000623     C                   endif
027600000623     C*
027700000623     C                   endif
027800000626     C                   endif
027900000623     C*
028000000710     C                   if        wrkesito<>*blanks
028100000630     C                   eval      vlrflg = '2'
028200000630     C                   endif
028300000630     C*
028400000623     C                   endsr
028500000711
028600000711
028700000711
028800990910      /TITLE Traduzione.
028900990910     C     traduci       begsr
029000990910     C* Chiamo il pgm traduttore.
029100990920     C                   call(e)   'TIS710C'
029200990920     C                   parm      tivlrds       prmtivlr
029300990910     C                   parm      *blanks       prmesito
029400000724     C                   parm      virfi3        prmlit
029500000710     C                   parm      virfi4        prmfir
029600021118     C                   parm      virfid        prmfid
029700051013     C                   parm                    prmmbrfit
029800080526     C                   parm                    prmtotrcd
029900990910     C*
030000990910     C                   select
030100000725     C* Se la call è finita in errore
030200000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
030300000725     C                   when      %error
030400000725     C                   eval      wrkesito = '1'
030500000725     C                   eval      vlrmsg = 'Errori non identificati' +
030600000725     C                                      ' in fase di trad.'
030700030613     C                   move      savMSG        vlrmsg
030800010615     C                   exsr      sndeml
030900000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
031000000725     C                   when      prmesito = '1'
031100990910     C                   eval      wrkesito = '1'
031200000710     C                   eval      vlrmsg = 'Invio effettuato,' +
031300000710     C                                      ' ma errori nella trad.'
031400030613     C                   move      savMSG        vlrmsg
031500000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
031600000725     C                   when      prmesito = '2'
031700000710     C                   eval      wrkesito = '1'
031800010615     C                   eval      vlrmsg = 'Errori in trad. ed' +
031900010615     C                                      ' invio non effettuato'
032000030613     C                   move      savMSG        vlrmsg
032100010615     C                   exsr      sndeml
032200000725     C* Se la sottomissione dell'invio al PO non è riuscita.
032300000725     C                   when      prmesito = '3'
032400990921     C                   eval      wrkesito = '1'
032500010618     C                   eval      vlrmsg = 'Errori sottomissione invio a PO.'
032600030613     C                   move      savMSG        vlrmsg
032700010615     C                   exsr      sndeml
032800010615     C* Se la il pgm traduttore non esiste.
032900010615     C                   when      prmesito = '4'
033000010615     C                   eval      wrkesito = '1'
033100010615     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
033200030613     C                   move      savMSG        vlrmsg
033300010615     C                   exsr      sndeml
033400010615     C* Se la chiamata al pgm traduttore fallisce.
033500010615     C                   when      prmesito = '5'
033600010615     C                   eval      wrkesito = '1'
033700010618     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
033800030613     C                   move      savMSG        vlrmsg
033900010615     C                   exsr      sndeml
034000010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
034100010615     C                   when      prmesito = '6'
034200010615     C                   eval      wrkesito = '1'
034300010618     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
034400010618     C                                      'invio effettuato ma verif.'
034500030613     C                   move      savMSG        vlrmsg
034600010615     C                   exsr      sndeml
034700021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
034800021118     C                   when      prmesito = '7'
034900021118     C                   eval      wrkesito = '1'
035000021118     C                   eval      vlrmsg = 'Errori in storicizzazione '
035100030613     C                   move      savMSG        vlrmsg
035200021118     C                   exsr      sndeml
035300030710     C* Se la traduzione di EDIVAB/EDIVAD ha avuto degli errori (occorre verificare).
035400030710     C                   when      prmesito = '8'
035500030710     C                   eval      wrkesito = '1'
035600030710     C                   eval      vlrmsg = 'Errore, nome file ' +
035700030710     C                                      'o parametri mancanti.'
035800030710     C                   move      savMSG        vlrmsg
035900030710     C                   exsr      sndeml
036000000725     C* Se la traduzione è stata eseguita senza errori.
036100990910     C                   when      not %error and prmesito = '0'
036200990910     C                   eval      wrkesito = '0'
036300000725     C                   eval      vlrmsg = 'Upload eseguito con successo.'
036400030613     C                   move      savMSG        vlrmsg
036500990910     C                   endsl
036600080526     C*
036700080526     C* Verifico in anagrafica clienti/tipi-file se richiesto l'invio della mail
036800080527     C* conferma ricezione file BARTVAS (solo se NN riesecuzione da log)
036900080527     C                   eval      dsvlr3 = vlrFL3
037000080527     C                   if        §VLR31 = *blanks
037100080526     C                   eval      vireTRC = '01'
037200080526     C     KEYvire1_C    chain     tivire1l
037300080526     C                   if        %found(tivire1l)
037400080526     C                   eval      dvire01 = vireFLO
037500080526     C                   if        §VIRE01INV = 'S'
037600080526     C                   call      'TIS7EMLR4'
037700080526     C                   parm                    virKSC
037800080527     C                   parm                    savMSG
037900080526     C                   parm                    prmtotrcd
038000080526     C                   parm                    §VIRE01EML
038100080526     C                   endif
038200080527     C                   endif
038300080526     C                   endif
038400990910     C*
038500990910     C                   endsr
038600990910
038700000626
038800000626
038900000626      /TITLE Inizializzazioni variabili.
039000000626     C     inzvar        begsr
039100000626     C*
039200000626     C                   time                    dataiso
039300000626     C     *iso          move      dataiso       data8
039400000626     C                   time                    ora
039500000626     C*
039600000626     C                   endsr
039700000626
039800010615
039900010615
040000010615
040100010615      /TITLE Compongo il testo e spedisco una e-m@ail
040200010615     C     sndeml        begsr
040300010615     C*
040400010615     C* Inizializzo variabili
040500010911     C                   movel     *blanks       wrkEml          253
040600010911     C                   movel     *blanks       wrkMsg         5000
040700010911     C                   movel     *blanks       wrkOgg           44
040800010615     C* Valorizzo i campi della e-m@ail
040900120305     C                   eval      wrkEml='cedalert@brt.it'
041000010911     C                   eval      wrkOgg='VAS - WARNING!!!'
041100010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
041200010615     C                             'SU AS/400'                         +':/N'+
041300010615     C                             ' '                                 +':/N'+
041400010615     C                             'RIFERIMENTI'                       +':/N'+
041500010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
041600010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
041700010615     C                             'Cliente:'+vlrksc                   +':/N'+
041800010615     C                             'TipoFile:'+vlrtip                  +':/N'+
041900010615     C                             'Traduttore:'+vlrpgt                +':/N'+
042000010615     C                             'Parametri:'+vlrppt                 +':/N'+
042100010615     C                             ' '                                 +':/N'+
042200010615     C                             'MESSAGGIO'                         +':/N'+
042300010615     C                             vlrmsg
042400010615     C*
042500010615     C                   call(e)   'TIS701C'
042600010615     C                   parm                    wrkEml
042700010911     C                   parm                    wrkOgg
042800010615     C                   parm                    wrkMsg
042900020118     C*
043000020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
043100080522     C                   if        %lookup(%subst(vlrksc:2:3):skLOG) > *zeros
043200020118     C*
043300120305     C                   eval      wrkEml='cedlog@brt.it'
043400020118     C*
043500020118     C                   call(e)   'TIS701C'
043600020118     C                   parm                    wrkEml
043700020118     C                   parm                    wrkOgg
043800020118     C                   parm                    wrkMsg
043900020118     C*
044000020118     C                   endif
044100020118     C* =====================================================
044200010615     C*
044300010615     C                   endsr
044400051108
044500051108
044600051108
044700051108      /TITLE Reperisce la data ora correnti x indicazione esatta elaborazione
044800051108     C     momentoElab   begsr
044900051108     C*
045000051108     C* Reperimento data corrente
045100051108     C                   time                    wn14             14 0
045200051108     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
045300051108     C                   movel     wn14          oracorElab        6 0          *ORA (6) IN HH/MM/SS
045400051108     C                   z-add     wn8           g08dat
045500051108     C                   z-add     *zeros        g08inv
045600051108     C                   movel     '0'           g08err
045700051108     C                   call      'XSRDA8'
045800051108     C                   parm                    wlbda8
045900051108     C                   z-add     g08inv        datcorElab        8 0          *DATA CORRENTE AA/M/
046000051108     C*
046100051108     C* Compongo la data/ora in stringa grosso modo leggibile occupando meno spazio possibile
046200051108     C                   movel     *all'-'       dataoraElab      15
046300051108     C                   movel     datcorElab    dataoraElab
046400051108     C                   move      oracorElab    dataoraElab
046500051108     C*
046600051108     C                   endsr
046700010615
046800010615
046900000626
047000000626
047100990908      /TITLE Operazioni iniziali.
047200990908     C     *inzsr        begsr
047300990908     C*
047400990908     C     *entry        plist
047500990908     C                   parm                    tivlrnrrp
047600000710     C                   parm                    virpgi
047700000710     C                   parm                    virppi
047800000717     C                   parm                    virfit
047900000717     C                   parm                    virfi3
048000000717     C                   parm                    virfi4
048100020107     C*
048200020107     C* Reperimento data corrente
048300020107     C                   time                    wn14             14 0
048400020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048500020107     C                   z-add     wn8           g08dat
048600020107     C                   z-add     *zeros        g08inv
048700020107     C                   movel     '0'           g08err
048800020107     C                   call      'XSRDA8'
048900020107     C                   parm                    wlbda8
049000020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049100080522     C*
049200080522     C* Caricamento filiali network Logistica
049300080522     C                   clear                   skLOG
049400080522     C                   z-add     *zeros        jLOG
049500080522     C     *start        setll     azorg01l
049600080522     C                   read      azorg01l
049700080522     C                   dow       not %eof(azorg01l)
049800080522     C                   eval      og143 = orgDE3
049900080522     C                   if        §OGNTW = 'LOG'
050000080522     C                   add       1             jLOG
050100080522     C                   movel     orgFIL        skLOG(jLOG)
050200080522     C                   endif
050300080522     C                   read      azorg01l
050400080522     C                   enddo
050500990920     C*
050600990920     C     k01vtf01      klist
050700990920     C                   kfld                    vtftip
050800000626     C*
050900000626     C     k03vir02      klist
051000000626     C                   kfld                    virksc
051100000626     C                   kfld                    virtip
051200000626     C                   kfld                    virdti
051300000626     C*
051400000626     C     k02vir02      klist
051500000626     C                   kfld                    virksc
051600000626     C                   kfld                    virtip
051700000626     C*
051800000626     C     k02vss01      klist
051900000626     C                   kfld                    vsssun
052000020107     C                   kfld                    vssisv
052100080526     C*
052200080526     C     KEYvire1_C    klist
052300080526     C                   kfld                    virksc
052400080526     C                   kfld                    virtip
052500080526     C                   kfld                    virdti
052600080526     C                   kfld                    vireTRC
052700990908     C*
052800990908     C                   endsr
