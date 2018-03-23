000100990910      /TITLE Upload via Internet: decisore.
000200990908     H dftactgrp(*yes)
000300990908
000400990908     Ftivlr00f  uf   e             disk
000500990920     Ftivtf01l  if   e           k disk
000600000623     Ftivir02l  if   e           k disk
000700000711     Ftivss01l  if   e           k disk
000800000711     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000900000627     F                                     PREFIX(f_)
001000000711     F                                     USROPN
001100990908
001200990910     D dscmz         e ds                  inz
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400990920     D tivlrnrrp       s             10s 0
001500021119     D sv_tivlrds    e ds                  extname(tivlr00f) prefix(sv_)
001600030617     D sv2_tivlrds   e ds                  extname(tivlr00f) prefix(sv2_)
001700990921     D prmtivlr        s                   like(tivlrds)
001800990910     D prmesito        s              1
001900000724     D prmlit          s             10    inz(*blanks)
002000000710     D prmfir          s             10    inz(*blanks)
002100021118     D prmfid          s             10    inz(*blanks)
002200051013     D prmmbrfit       s              1    inz(*blanks)
002300990910     D wrkesito        s              1
002400000626     D dataiso         s               d
002500000626     D dataiso1        s               d
002600000626     D data8           s              8  0
002700000626     D ora             s              6  0
002800000626     D flgvirfound     s              1    inz('0')
002900000703     D wrkprg          s              8  0 inz(*zeros)
003000000703     D dwlprg          s             10    inz(*all'0')
003100030616     D savMSG          s             30
003200020107
003300020107     D*------------------
003400020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500020107     D*------------------
003600020107     D wlbda8          DS                  INZ
003700020107     D  g08dat                 1      8  0
003800020107     D  g08inv                 9     16  0
003900020107     D  g08err                17     17
004000020107     D  g08tgi                18     22  0
004100020107     D*
004200000626
004300000626
004400990908     C* Leggo il record di log.
004500990908     C                   dou       %status = 0
004600990908     C     tivlrnrrp     chain(e)  tivlr00f
004700990908     C                   enddo
004800030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
004900030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
005000030613     C                   if        %len(%trim(vlrmsg)) > 0
005100030613     C                   eval      savMSG = %trim(vlrmsg)
005200030613     C                   endif
005300000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
005400000630     C                   exsr      inzvar
005500000711     C                   exsr      readtivir
005600021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
005700021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
005800021115     C                   eval      vlrpoi = virpoi
005900021115     C                   eval      vlrfl1 = virfi2
006000021115     C                   endif
006100010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
006200010710     C* ... o ancora da data-entry giudato via Internet)
006300000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
006400000703     C                             vlrfis = *blanks or
006500011127     C                             vlrfou = *blanks or
006600011127     C                             vlrpgt = *blanks
006700000711     C* Completo i campi mancanti nel record di log
006800000711     C                   eval      vlrlis = '*LIBL'
006900000711     C                   eval      vlrfis = virfis
007000011127     C                   eval      vlrfou = virfit
007100000711     C                   eval      vlrpgt = virpgt
007200000711     C                   eval      vlrppt = virppt
007300000626     C                   else
007400000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
007500010524     C                   if        vlrprg = *blanks
007600000627     C                   exsr      calprog
007700000627     C                   endif
007800000626     C                   clear                   wrkesito
007900030616     C***                clear                   vlrmsg
008000030616     C                   move      savMSG        vlrmsg
008100021119     C*
008200021119     C* Prima della traduzione disalloco il record
008300021119     C                   unlock    tivlr00f
008400021119     C*
008500990910     C* Traduzione.
008600990910     C                   exsr      traduci
008700021119     C*
008800030617     C* Subito dopo la traduzione salvo buffer di ritorno dalla traduzione che può essere cambiato
008900030617     C                   eval      sv2_tivlrds = prmtivlr
009000030617     C* Dopo la traduzione salvo anche il buffer originale del record e rileggo lo stesso record
009100030617     C                   eval      sv_tivlrds = tivlrds
009200021119     C                   dou       %status = 0
009300021119     C     tivlrnrrp     chain(e)  tivlr00f
009400021119     C                   enddo
009500021119     C*
009600030617     C* Dopo aver letto memorizzo gli unici campi che nella traduzione possono essere cambiati
009700030617     C* (vlrfl3 = flag operativi e vlrpoi = P.O. invio file tradotto)
009800030617     C* e re-imposto il buffer salvato prima della rilettura del record.
009900021119     C                   eval      sv_vlrfl3 = vlrfl3
010000021119     C                   eval      tivlrds = sv_tivlrds
010100030617     C                   eval      vlrpoi = sv2_vlrpoi
010200021119     C*
010300000626     C* Imposto stato record.
010400000626     C                   select
010500000626     C                   when      wrkesito = ' '
010600000626     C                   eval      vlrflg = '0'
010700000626     C                   when      wrkesito = '0'
010800000626     C                   eval      vlrflg = '1'
010900000626     C                   other
011000000626     C                   eval      vlrflg = '2'
011100000626     C                   endsl
011200010615     C*
011300000626     C                   endif
011400021115     C                   exsr      updtivlr
011500000626     C*
011600000627     C                   seton                                        lr
011700000626
011800000627
011900000626
012000000627      /TITLE Valorizzazione Progressivo Applicazione
012100000627     C     calprog       begsr
012200000627     C*
012300000711     C                   open(e)   tis7prgf
012400000711     C                   if        not %error
012500000627     C                   read      tis7prgf
012600000627     C*
012700000627     C                   eval      dwlprg = f_tis7prgf
012800000627     C                   move(p)   dwlprg        wrkprg
012900000627     C                   add       1             wrkprg
013000000627     C                   move(p)   wrkprg        dwlprg
013100000627     C                   movel     vlrisv        dwlprg
013200000627     C*
013300000627     C                   eval      f_tis7prgf = dwlprg
013400010524     C                   update(e) tis7prg0
013500010524     C                   if        not %error
013600010524     C                   eval      vlrprg = dwlprg
013700000711     C                   endif
013800010524     C                   endif
013900000711     C                   close(e)  tis7prgf
014000000627     C*
014100000627     C                   endsr
014200000627
014300000627
014400000626
014500000626      /TITLE Aggiornamento TIVLR00F.
014600000626     C     updtivlr      begsr
014700000630     C*
014800000626     C* Aggiorno record di log con l'esito dell'elaborazione.
014900000627     C*                  if        vlrflg <> '0'
015000000630     C*
015100051108     C* Nel percorso nome file ricevuto inserisco anche la data/ora d effettiva elaborazione
015200051108     C* (orribilis ma mi serve e qui posso metterlo senza dare fastidio a nulla)
015300051108     C                   exsr      momentoElab
015400051108     C                   move      dataoraElab   vlrpth
015500051108     C* Quindi aggiorno il record d log corrente
015600990910     C                   update    tivlr000
015700000627     C*                  else
015800000627     C*                  unlock    tivlr00f
015900000627     C*                  endif
016000000626     C*
016100000626     C                   endsr
016200000626
016300000626
016400000623
016500000623      /TITLE Lettura TIVIR00F.
016600000626     C     readtivir     begsr
016700000623     C*
016800000626     C                   clear                   wrkesito
016900000626     C*
017000000626     C                   eval      vsssun = vlrsun
017100020107     C                   eval      vssisv = vlrisv
017200000623     C     k02vss01      chain     tivss01l
017300000623     C*
017400020107     C                   if        not %found or
017500020107     C                             vssdde > datcor or
017600020107     C                             vssdsc < datcor
017700000630     C                   eval      wrkesito='1'
017800000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
017900000630     C                                      'al servizio.'
018000030613     C                   move      savMSG        vlrmsg
018100010615     C                   exsr      sndeml
018200000626     C                   else
018300010710     C*
018400010710     C* Costruisco la parte fissa della chiave
018500010710     C                   eval      virksc = vssksu
018600010710     C                   eval      virtip = vlrtip
018700010710     C                   move      dataiso       dataiso1
018800010710     C                   adddur    1:*d          dataiso1
018900010710     C     *iso          move      dataiso1      virdti
019000010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
019100010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
019200010710     C                   if        vlrtia = 'I'
019300010726 xxx C***                clear                   tivir000
019400010710     C                   eval      flgvirfound = *on
019500010710     C                   else
019600010710     C*
019700000623     C                   reset                   flgvirfound
019800000623     C     k03vir02      setll     tivir02l
019900000623     C     k02vir02      readpe    tivir02l
020000000623     C* Se ho letto il record
020100000623     C* ed è valido segnalo che l'ho trovato.
020200000623     C                   if        not %eof
020300000623     C                             and data8 <= virdtf
020400000623     C                   eval      flgvirfound = *on
020500000630     C                   else
020600000630     C                   if        flgvirfound = *off
020700000630     C                   eval      wrkesito='1'
020800000630     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
020900000630     C                                      ' per il cliente.'
021000030613     C                   move      savMSG        vlrmsg
021100010615     C                   exsr      sndeml
021200000623     C                   endif
021300000630     C                   endif
021400010626     C*
021500010626     C                   endif
021600010626     C* In ogni caso =>
021700010626     C* ... reperisco regole standard.
021800000623     C                   if        flgvirfound = '1'
021900000623     C*
022000010726     C                   eval      vtftip = virtip
022100000623     C     k01vtf01      chain     tivtf01l
022200000623     C*
022300000623     C                   if        %found
022400000623     C*
022500000623     C                   if        virctl = *blanks
022600000623     C                   eval      virctl = vtfctl
022700000623     C                   endif
022800000623     C*
022900000623     C                   if        virpgt = *blanks
023000000623     C                   eval      virpgt = vtfpgt
023100000623     C                   endif
023200000725     C*
023300000725     C                   if        virppt = *blanks
023400000725     C                   eval      virppt = vtfppt
023500000725     C                   endif
023600000713     C*
023700000713     C                   if        virpgi = *blanks
023800000713     C                   eval      virpgi = vtfpgi
023900000713     C                   endif
024000000725     C*
024100000725     C                   if        virppi = *blanks
024200000725     C                   eval      virppi = vtfppi
024300000725     C                   endif
024400000623     C*
024500000623     C                   if        virfid = *blanks
024600000623     C                   eval      virfid = vtffid
024700000623     C                   endif
024800000623     C*
024900000623     C                   if        virfis = *blanks
025000000623     C                   eval      virfis = vtffis
025100000623     C                   endif
025200000623     C*
025300000623     C                   if        virfit = *blanks
025400000623     C                   eval      virfit = vtffit
025500000623     C                   endif
025600051013     C*
025700051013     C                   eval      prmmbrfit = %subst(vtffi2:1:1)
025800000623     C*
025900000630     C                   else
026000000630     C                   eval      wrkesito='1'
026100000630     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
026200030613     C                   move      savMSG        vlrmsg
026300010615     C                   exsr      sndeml
026400000623     C                   endif
026500000623     C*
026600000623     C                   endif
026700000626     C                   endif
026800000623     C*
026900000710     C                   if        wrkesito<>*blanks
027000000630     C                   eval      vlrflg = '2'
027100000630     C                   endif
027200000630     C*
027300000623     C                   endsr
027400000711
027500000711
027600000711
027700990910      /TITLE Traduzione.
027800990910     C     traduci       begsr
027900990910     C* Chiamo il pgm traduttore.
028000990920     C                   call(e)   'TIS710C'
028100990920     C                   parm      tivlrds       prmtivlr
028200990910     C                   parm      *blanks       prmesito
028300000724     C                   parm      virfi3        prmlit
028400000710     C                   parm      virfi4        prmfir
028500021118     C                   parm      virfid        prmfid
028600051013     C                   parm                    prmmbrfit
028700990910     C*
028800990910     C                   select
028900000725     C* Se la call è finita in errore
029000000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
029100000725     C                   when      %error
029200000725     C                   eval      wrkesito = '1'
029300000725     C                   eval      vlrmsg = 'Errori non identificati' +
029400000725     C                                      ' in fase di trad.'
029500030613     C                   move      savMSG        vlrmsg
029600010615     C                   exsr      sndeml
029700000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
029800000725     C                   when      prmesito = '1'
029900990910     C                   eval      wrkesito = '1'
030000000710     C                   eval      vlrmsg = 'Invio effettuato,' +
030100000710     C                                      ' ma errori nella trad.'
030200030613     C                   move      savMSG        vlrmsg
030300000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
030400000725     C                   when      prmesito = '2'
030500000710     C                   eval      wrkesito = '1'
030600010615     C                   eval      vlrmsg = 'Errori in trad. ed' +
030700010615     C                                      ' invio non effettuato'
030800030613     C                   move      savMSG        vlrmsg
030900010615     C                   exsr      sndeml
031000000725     C* Se la sottomissione dell'invio al PO non è riuscita.
031100000725     C                   when      prmesito = '3'
031200990921     C                   eval      wrkesito = '1'
031300010618     C                   eval      vlrmsg = 'Errori sottomissione invio a PO.'
031400030613     C                   move      savMSG        vlrmsg
031500010615     C                   exsr      sndeml
031600010615     C* Se la il pgm traduttore non esiste.
031700010615     C                   when      prmesito = '4'
031800010615     C                   eval      wrkesito = '1'
031900010615     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
032000030613     C                   move      savMSG        vlrmsg
032100010615     C                   exsr      sndeml
032200010615     C* Se la chiamata al pgm traduttore fallisce.
032300010615     C                   when      prmesito = '5'
032400010615     C                   eval      wrkesito = '1'
032500010618     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
032600030613     C                   move      savMSG        vlrmsg
032700010615     C                   exsr      sndeml
032800010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
032900010615     C                   when      prmesito = '6'
033000010615     C                   eval      wrkesito = '1'
033100010618     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
033200010618     C                                      'invio effettuato ma verif.'
033300030613     C                   move      savMSG        vlrmsg
033400010615     C                   exsr      sndeml
033500021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
033600021118     C                   when      prmesito = '7'
033700021118     C                   eval      wrkesito = '1'
033800021118     C                   eval      vlrmsg = 'Errori in storicizzazione '
033900030613     C                   move      savMSG        vlrmsg
034000021118     C                   exsr      sndeml
034100030710     C* Se la traduzione di EDIVAB/EDIVAD ha avuto degli errori (occorre verificare).
034200030710     C                   when      prmesito = '8'
034300030710     C                   eval      wrkesito = '1'
034400030710     C                   eval      vlrmsg = 'Errore, nome file ' +
034500030710     C                                      'o parametri mancanti.'
034600030710     C                   move      savMSG        vlrmsg
034700030710     C                   exsr      sndeml
034800000725     C* Se la traduzione è stata eseguita senza errori.
034900990910     C                   when      not %error and prmesito = '0'
035000990910     C                   eval      wrkesito = '0'
035100000725     C                   eval      vlrmsg = 'Upload eseguito con successo.'
035200030613     C                   move      savMSG        vlrmsg
035300990910     C                   endsl
035400990910     C*
035500990910     C                   endsr
035600990910
035700000626
035800000626
035900000626      /TITLE Inizializzazioni variabili.
036000000626     C     inzvar        begsr
036100000626     C*
036200000626     C                   time                    dataiso
036300000626     C     *iso          move      dataiso       data8
036400000626     C                   time                    ora
036500000626     C*
036600000626     C                   endsr
036700000626
036800010615
036900010615
037000010615
037100010615      /TITLE Compongo il testo e spedisco una e-m@ail
037200010615     C     sndeml        begsr
037300010615     C*
037400010615     C* Inizializzo variabili
037500010911     C                   movel     *blanks       wrkEml          253
037600010911     C                   movel     *blanks       wrkMsg         5000
037700010911     C                   movel     *blanks       wrkOgg           44
037800010615     C* Valorizzo i campi della e-m@ail
037900120305     C                   eval      wrkEml='cedalert@brt.it'
038000010911     C                   eval      wrkOgg='VAS - WARNING!!!'
038100010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
038200010615     C                             'SU AS/400'                         +':/N'+
038300010615     C                             ' '                                 +':/N'+
038400010615     C                             'RIFERIMENTI'                       +':/N'+
038500010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
038600010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
038700010615     C                             'Cliente:'+vlrksc                   +':/N'+
038800010615     C                             'TipoFile:'+vlrtip                  +':/N'+
038900010615     C                             'Traduttore:'+vlrpgt                +':/N'+
039000010615     C                             'Parametri:'+vlrppt                 +':/N'+
039100010615     C                             ' '                                 +':/N'+
039200010615     C                             'MESSAGGIO'                         +':/N'+
039300010615     C                             vlrmsg
039400010615     C*
039500010615     C                   call(e)   'TIS701C'
039600010615     C                   parm                    wrkEml
039700010911     C                   parm                    wrkOgg
039800010615     C                   parm                    wrkMsg
039900020118     C*
040000020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
040100020118     C                   if        %subst(vlrksc:2:3) = '199' or
040200020118     C                             %subst(vlrksc:2:3) = '200' or
040300020118     C                             %subst(vlrksc:2:3) = '207' or
040400031008     C                             %subst(vlrksc:2:3) = '208' or
040500031008     C                             %subst(vlrksc:2:3) = '212'
040600020118     C*
040700120305     C                   eval      wrkEml='cedlog@brt.it'
040800020118     C*
040900020118     C                   call(e)   'TIS701C'
041000020118     C                   parm                    wrkEml
041100020118     C                   parm                    wrkOgg
041200020118     C                   parm                    wrkMsg
041300020118     C*
041400020118     C                   endif
041500020118     C* =====================================================
041600010615     C*
041700010615     C                   endsr
041800051108
041900051108
042000051108
042100051108      /TITLE Reperisce la data ora correnti x indicazione esatta elaborazione
042200051108     C     momentoElab   begsr
042300051108     C*
042400051108     C* Reperimento data corrente
042500051108     C                   time                    wn14             14 0
042600051108     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042700051108     C                   movel     wn14          oracorElab        6 0          *ORA (6) IN HH/MM/SS
042800051108     C                   z-add     wn8           g08dat
042900051108     C                   z-add     *zeros        g08inv
043000051108     C                   movel     '0'           g08err
043100051108     C                   call      'XSRDA8'
043200051108     C                   parm                    wlbda8
043300051108     C                   z-add     g08inv        datcorElab        8 0          *DATA CORRENTE AA/M/
043400051108     C*
043500051108     C* Compongo la data/ora in stringa grosso modo leggibile occupando meno spazio possibile
043600051108     C                   movel     *all'-'       dataoraElab      15
043700051108     C                   movel     datcorElab    dataoraElab
043800051108     C                   move      oracorElab    dataoraElab
043900051108     C*
044000051108     C                   endsr
044100010615
044200010615
044300000626
044400000626
044500990908      /TITLE Operazioni iniziali.
044600990908     C     *inzsr        begsr
044700990908     C*
044800990908     C     *entry        plist
044900990908     C                   parm                    tivlrnrrp
045000000710     C                   parm                    virpgi
045100000710     C                   parm                    virppi
045200000717     C                   parm                    virfit
045300000717     C                   parm                    virfi3
045400000717     C                   parm                    virfi4
045500020107     C*
045600020107     C* Reperimento data corrente
045700020107     C                   time                    wn14             14 0
045800020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
045900020107     C                   z-add     wn8           g08dat
046000020107     C                   z-add     *zeros        g08inv
046100020107     C                   movel     '0'           g08err
046200020107     C                   call      'XSRDA8'
046300020107     C                   parm                    wlbda8
046400020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
046500990920     C*
046600990920     C     k01vtf01      klist
046700990920     C                   kfld                    vtftip
046800000626     C*
046900000626     C     k03vir02      klist
047000000626     C                   kfld                    virksc
047100000626     C                   kfld                    virtip
047200000626     C                   kfld                    virdti
047300000626     C*
047400000626     C     k02vir02      klist
047500000626     C                   kfld                    virksc
047600000626     C                   kfld                    virtip
047700000626     C*
047800000626     C     k02vss01      klist
047900000626     C                   kfld                    vsssun
048000020107     C                   kfld                    vssisv
048100990908     C*
048200990908     C                   endsr
