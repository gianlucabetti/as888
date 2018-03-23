000100090407      *PARMS DYNUSRPRF(*OWNER)
000200990910      /TITLE Upload via Internet: decisore.
000300130114     H dftactgrp(*no) actgrp(*caller)
000400990908
000500080522     Fazorg01l  if   e           k disk
000600990908     Ftivlr00f  uf   e             disk
000700990920     Ftivtf01l  if   e           k disk
000800000623     Ftivir02l  if   e           k disk
000900080526     Ftivire1l  if   e           k disk
001000000711     Ftivss01l  if   e           k disk
001100000711     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001200000627     F                                     PREFIX(f_)
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
004000090407     D conta           s              2  0
004100090407     D contaa          s              2
004200020107
004300020107     D*------------------
004400020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500020107     D*------------------
004600020107     D wlbda8          DS                  INZ
004700020107     D  g08dat                 1      8  0
004800020107     D  g08inv                 9     16  0
004900020107     D  g08err                17     17
005000020107     D  g08tgi                18     22  0
005100020107     D*
005200000626
005300000626
005400090407     C*
005500090407     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005600090407     C
005700090407     C/EXEC SQL
005800090407     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005900090407     C/END-EXEC
006000090407     C*
006100990908     C* Leggo il record di log.
006200990908     C                   dou       %status = 0
006300990908     C     tivlrnrrp     chain(e)  tivlr00f
006400990908     C                   enddo
006500030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
006600030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
006700030613     C                   if        %len(%trim(vlrmsg)) > 0
006800090116     C                   if        %subst(vlrfl3:1:1) = '1'
006900090116     C                   eval      savMSG = %subst(vlrmsg:51)
007000090116     C                   else
007100080522     C                   eval      savMSG = vlrmsg
007200090116     C                   endif
007300030613     C                   endif
007400000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
007500000630     C                   exsr      inzvar
007600000711     C                   exsr      readtivir
007700021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
007800021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
007900021115     C                   eval      vlrpoi = virpoi
008000021115     C                   eval      vlrfl1 = virfi2
008100021115     C                   endif
008200010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
008300010710     C* ... o ancora da data-entry giudato via Internet)
008400000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
008500000703     C                             vlrfis = *blanks or
008600011127     C                             vlrfou = *blanks or
008700011127     C                             vlrpgt = *blanks
008800000711     C* Completo i campi mancanti nel record di log
008900000711     C                   eval      vlrlis = '*LIBL'
009000000711     C                   eval      vlrfis = virfis
009100011127     C                   eval      vlrfou = virfit
009200000711     C                   eval      vlrpgt = virpgt
009300000711     C                   eval      vlrppt = virppt
009400000626     C                   else
009500090407     C*
009600090407     C* Verifico elaborabilità transazione (completa)
009700090407     C                   exsr      chkElab
009800090407     C*
009900090407     C* Se transazione (completa) elaborabile => procedo
010000090407     C                   if        *in55 = *on
010100090407     C*
010200000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
010300010524     C                   if        vlrprg = *blanks
010400000627     C                   exsr      calprog
010500000627     C                   endif
010600000626     C                   clear                   wrkesito
010700030616     C***                clear                   vlrmsg
010800030616     C                   move      savMSG        vlrmsg
010900021119     C*
011000021119     C* Prima della traduzione disalloco il record
011100021119     C                   unlock    tivlr00f
011200021119     C*
011300990910     C* Traduzione.
011400990910     C                   exsr      traduci
011500021119     C*
011600030617     C* Subito dopo la traduzione salvo buffer di ritorno dalla traduzione che può essere cambiato
011700030617     C                   eval      sv2_tivlrds = prmtivlr
011800030617     C* Dopo la traduzione salvo anche il buffer originale del record e rileggo lo stesso record
011900030617     C                   eval      sv_tivlrds = tivlrds
012000021119     C                   dou       %status = 0
012100021119     C     tivlrnrrp     chain(e)  tivlr00f
012200021119     C                   enddo
012300021119     C*
012400030617     C* Dopo aver letto memorizzo gli unici campi che nella traduzione possono essere cambiati
012500030617     C* (vlrfl3 = flag operativi e vlrpoi = P.O. invio file tradotto)
012600030617     C* e re-imposto il buffer salvato prima della rilettura del record.
012700021119     C                   eval      sv_vlrfl3 = vlrfl3
012800021119     C                   eval      tivlrds = sv_tivlrds
012900030617     C                   eval      vlrpoi = sv2_vlrpoi
013000021119     C*
013100000626     C* Imposto stato record.
013200000626     C                   select
013300000626     C                   when      wrkesito = ' '
013400000626     C                   eval      vlrflg = '0'
013500000626     C                   when      wrkesito = '0'
013600000626     C                   eval      vlrflg = '1'
013700000626     C                   other
013800000626     C                   eval      vlrflg = '2'
013900000626     C                   endsl
014000090407     C*
014100090407     C                   endif
014200010615     C*
014300000626     C                   endif
014400021115     C                   exsr      updtivlr
014500000626     C*
014600000627     C                   seton                                        lr
014700090407
014800090407
014900090407
015000000626
015100090407
015200090407      /TITLE Verifica elaborabilità
015300090407     C     chkElab       begsr
015400090407     C*
015500090407     C                   setoff                                       55
015600090407     C*
015700090407     C/EXEC SQL
015800090407     C+ declare C1 cursor for select count(*)
015900090407     C+ from tivlr00f where
016000090407     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
016100090421     C+ and vlrflg <> 'M'
016200090407     C+ and vlrpoi <> 0
016300090504     C+ and ((vlrsqz = :vlrsqz and vlrflg not in ('3', '4'))
016400090504     C+ or vlrflg='0')
016500090407     C+ for fetch only
016600090407     C/END-EXEC
016700090407     C
016800090407     C/EXEC SQL
016900090407     C+ open C1
017000090407     C/END-EXEC
017100090407     C
017200090407     C/EXEC SQL
017300090407     C+ Fetch C1 into :conta
017400090407     C/END-EXEC
017500090407     C*
017600090407     C                   move      conta         contaa
017700090407     C*
017800090407     C                   if        sqlcod = *zeros and
017900090407     C                             contaa = vlrfl2
018000090407     C                   seton                                        55
018100090407     C                   endif
018200090407     C
018300090407     C/EXEC SQL
018400090407     C+ close C1
018500090407     C/END-EXEC
018600090407     C
018700090407     C*
018800090407     C                   endsr
018900090407
019000090407
019100000627
019200000626
019300000627      /TITLE Valorizzazione Progressivo Applicazione
019400000627     C     calprog       begsr
019500170324     C*
019600170324     C     *start        setll     tis7prgf
019700170324     C                   read(e)   tis7prgf
019800170324     C                   if        not %error
019900000627     C*
020000000627     C                   eval      dwlprg = f_tis7prgf
020100000627     C                   move(p)   dwlprg        wrkprg
020200000627     C                   add       1             wrkprg
020300000627     C                   move(p)   wrkprg        dwlprg
020400000627     C                   movel     vlrisv        dwlprg
020500000627     C*
020600000627     C                   eval      f_tis7prgf = dwlprg
020700010524     C                   update(e) tis7prg0
020800010524     C                   if        not %error
020900010524     C                   eval      vlrprg = dwlprg
021000000711     C                   endif
021100010524     C                   endif
021200000627     C*
021300000627     C                   endsr
021400000627
021500000627
021600000626
021700000626      /TITLE Aggiornamento TIVLR00F.
021800000626     C     updtivlr      begsr
021900000630     C*
022000000626     C* Aggiorno record di log con l'esito dell'elaborazione.
022100000627     C*                  if        vlrflg <> '0'
022200000630     C*
022300051108     C* Nel percorso nome file ricevuto inserisco anche la data/ora d effettiva elaborazione
022400051108     C* (orribilis ma mi serve e qui posso metterlo senza dare fastidio a nulla)
022500051108     C                   exsr      momentoElab
022600051108     C                   move      dataoraElab   vlrpth
022700051108     C* Quindi aggiorno il record d log corrente
022800990910     C                   update    tivlr000
022900000627     C*                  else
023000000627     C*                  unlock    tivlr00f
023100000627     C*                  endif
023200000626     C*
023300000626     C                   endsr
023400000626
023500000626
023600000623
023700000623      /TITLE Lettura TIVIR00F.
023800000626     C     readtivir     begsr
023900000623     C*
024000000626     C                   clear                   wrkesito
024100000626     C*
024200000626     C                   eval      vsssun = vlrsun
024300020107     C                   eval      vssisv = vlrisv
024400000623     C     k02vss01      chain     tivss01l
024500000623     C*
024600020107     C                   if        not %found or
024700020107     C                             vssdde > datcor or
024800020107     C                             vssdsc < datcor
024900000630     C                   eval      wrkesito='1'
025000000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
025100000630     C                                      'al servizio.'
025200030613     C                   move      savMSG        vlrmsg
025300010615     C                   exsr      sndeml
025400000626     C                   else
025500010710     C*
025600010710     C* Costruisco la parte fissa della chiave
025700010710     C                   eval      virksc = vssksu
025800010710     C                   eval      virtip = vlrtip
025900010710     C                   move      dataiso       dataiso1
026000010710     C                   adddur    1:*d          dataiso1
026100010710     C     *iso          move      dataiso1      virdti
026200010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
026300010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
026400010710     C                   if        vlrtia = 'I'
026500010710     C                   eval      flgvirfound = *on
026600010710     C                   else
026700080522     C                   eval      flgvirfound = *off
026800080522     C                   endif
026900080522     C*
027000000623     C     k03vir02      setll     tivir02l
027100000623     C     k02vir02      readpe    tivir02l
027200000623     C* Se ho letto il record
027300000623     C* ed è valido segnalo che l'ho trovato.
027400000623     C                   if        not %eof
027500000623     C                             and data8 <= virdtf
027600000623     C                   eval      flgvirfound = *on
027700000630     C                   else
027800000630     C                   if        flgvirfound = *off
027900000630     C                   eval      wrkesito='1'
028000000630     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
028100000630     C                                      ' per il cliente.'
028200030613     C                   move      savMSG        vlrmsg
028300010615     C                   exsr      sndeml
028400000623     C                   endif
028500000630     C                   endif
028600010626     C*
028700010626     C* In ogni caso =>
028800010626     C* ... reperisco regole standard.
028900000623     C                   if        flgvirfound = '1'
029000000623     C*
029100010726     C                   eval      vtftip = virtip
029200000623     C     k01vtf01      chain     tivtf01l
029300000623     C*
029400000623     C                   if        %found
029500000623     C*
029600000623     C                   if        virctl = *blanks
029700000623     C                   eval      virctl = vtfctl
029800000623     C                   endif
029900000623     C*
030000000623     C                   if        virpgt = *blanks
030100000623     C                   eval      virpgt = vtfpgt
030200000623     C                   endif
030300000725     C*
030400000725     C                   if        virppt = *blanks
030500000725     C                   eval      virppt = vtfppt
030600000725     C                   endif
030700000713     C*
030800000713     C                   if        virpgi = *blanks
030900000713     C                   eval      virpgi = vtfpgi
031000000713     C                   endif
031100000725     C*
031200101013     C***                if        virppi = *blanks
031300101013     C***                eval      virppi = vtfppi
031400101013     C***                endif
031500000623     C*
031600000623     C                   if        virfid = *blanks
031700000623     C                   eval      virfid = vtffid
031800000623     C                   endif
031900000623     C*
032000000623     C                   if        virfis = *blanks
032100000623     C                   eval      virfis = vtffis
032200000623     C                   endif
032300000623     C*
032400000623     C                   if        virfit = *blanks
032500000623     C                   eval      virfit = vtffit
032600000623     C                   endif
032700051013     C*
032800051013     C                   eval      prmmbrfit = %subst(vtffi2:1:1)
032900000623     C*
033000000630     C                   else
033100000630     C                   eval      wrkesito='1'
033200000630     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
033300030613     C                   move      savMSG        vlrmsg
033400010615     C                   exsr      sndeml
033500000623     C                   endif
033600000623     C*
033700000623     C                   endif
033800000626     C                   endif
033900000623     C*
034000000710     C                   if        wrkesito<>*blanks
034100000630     C                   eval      vlrflg = '2'
034200000630     C                   endif
034300000630     C*
034400000623     C                   endsr
034500000711
034600000711
034700000711
034800990910      /TITLE Traduzione.
034900990910     C     traduci       begsr
035000990910     C* Chiamo il pgm traduttore.
035100990920     C                   call(e)   'TIS710C'
035200990920     C                   parm      tivlrds       prmtivlr
035300990910     C                   parm      *blanks       prmesito
035400000724     C                   parm      virfi3        prmlit
035500000710     C                   parm      virfi4        prmfir
035600021118     C                   parm      virfid        prmfid
035700051013     C                   parm                    prmmbrfit
035800080526     C                   parm                    prmtotrcd
035900990910     C*
036000990910     C                   select
036100000725     C* Se la call è finita in errore
036200000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
036300000725     C                   when      %error
036400000725     C                   eval      wrkesito = '1'
036500000725     C                   eval      vlrmsg = 'Errori non identificati' +
036600000725     C                                      ' in fase di trad.'
036700030613     C                   move      savMSG        vlrmsg
036800010615     C                   exsr      sndeml
036900000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
037000000725     C                   when      prmesito = '1'
037100990910     C                   eval      wrkesito = '1'
037200000710     C                   eval      vlrmsg = 'Invio effettuato,' +
037300000710     C                                      ' ma errori nella trad.'
037400030613     C                   move      savMSG        vlrmsg
037500000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
037600000725     C                   when      prmesito = '2'
037700000710     C                   eval      wrkesito = '1'
037800010615     C                   eval      vlrmsg = 'Errori in trad. ed' +
037900010615     C                                      ' invio non effettuato'
038000030613     C                   move      savMSG        vlrmsg
038100010615     C                   exsr      sndeml
038200170519     C* Se la sottomissione dell'invio al PO non è riuscita OPPURE in fase di applicazione
038300170519     C* personalizzazioni cliente previste in tabella VPC.
038400000725     C                   when      prmesito = '3'
038500990921     C                   eval      wrkesito = '1'
038600170519     C                   eval      vlrmsg = 'Errori invio a FIL ' +
038700170519     C                                      'o personalizz. VPC'
038800030613     C                   move      savMSG        vlrmsg
038900010615     C                   exsr      sndeml
039000010615     C* Se la il pgm traduttore non esiste.
039100010615     C                   when      prmesito = '4'
039200010615     C                   eval      wrkesito = '1'
039300010615     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
039400030613     C                   move      savMSG        vlrmsg
039500010615     C                   exsr      sndeml
039600010615     C* Se la chiamata al pgm traduttore fallisce.
039700010615     C                   when      prmesito = '5'
039800010615     C                   eval      wrkesito = '1'
039900010618     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
040000030613     C                   move      savMSG        vlrmsg
040100010615     C                   exsr      sndeml
040200010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
040300010615     C                   when      prmesito = '6'
040400010615     C                   eval      wrkesito = '1'
040500010618     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
040600010618     C                                      'invio effettuato ma verif.'
040700030613     C                   move      savMSG        vlrmsg
040800010615     C                   exsr      sndeml
040900021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
041000021118     C                   when      prmesito = '7'
041100021118     C                   eval      wrkesito = '1'
041200021118     C                   eval      vlrmsg = 'Errori in storicizzazione '
041300030613     C                   move      savMSG        vlrmsg
041400021118     C                   exsr      sndeml
041500030710     C* Se la traduzione di EDIVAB/EDIVAD ha avuto degli errori (occorre verificare).
041600030710     C                   when      prmesito = '8'
041700030710     C                   eval      wrkesito = '1'
041800030710     C                   eval      vlrmsg = 'Errore, nome file ' +
041900030710     C                                      'o parametri mancanti.'
042000030710     C                   move      savMSG        vlrmsg
042100030710     C                   exsr      sndeml
042200000725     C* Se la traduzione è stata eseguita senza errori.
042300990910     C                   when      not %error and prmesito = '0'
042400990910     C                   eval      wrkesito = '0'
042500000725     C                   eval      vlrmsg = 'Upload eseguito con successo.'
042600030613     C                   move      savMSG        vlrmsg
042700990910     C                   endsl
042800080526     C*
042900080526     C* Verifico in anagrafica clienti/tipi-file se richiesto l'invio della mail
043000080527     C* conferma ricezione file BARTVAS (solo se NN riesecuzione da log)
043100080527     C                   eval      dsvlr3 = vlrFL3
043200080527     C                   if        §VLR31 = *blanks
043300080526     C                   eval      vireTRC = '01'
043400080526     C     KEYvire1_C    chain     tivire1l
043500080526     C                   if        %found(tivire1l)
043600080526     C                   eval      dvire01 = vireFLO
043700080526     C                   if        §VIRE01INV = 'S'
043800080526     C                   call      'TIS7EMLR4'
043900080526     C                   parm                    virKSC
044000080527     C                   parm                    savMSG
044100080526     C                   parm                    prmtotrcd
044200080526     C                   parm                    §VIRE01EML
044300080526     C                   endif
044400080527     C                   endif
044500080526     C                   endif
044600990910     C*
044700990910     C                   endsr
044800990910
044900000626
045000000626
045100000626      /TITLE Inizializzazioni variabili.
045200000626     C     inzvar        begsr
045300000626     C*
045400000626     C                   time                    dataiso
045500000626     C     *iso          move      dataiso       data8
045600000626     C                   time                    ora
045700000626     C*
045800000626     C                   endsr
045900000626
046000010615
046100010615
046200010615
046300010615      /TITLE Compongo il testo e spedisco una e-m@ail
046400010615     C     sndeml        begsr
046500010615     C*
046600010615     C* Inizializzo variabili
046700010911     C                   movel     *blanks       wrkEml          253
046800010911     C                   movel     *blanks       wrkMsg         5000
046900010911     C                   movel     *blanks       wrkOgg           44
047000010615     C* Valorizzo i campi della e-m@ail
047100120305     C                   eval      wrkEml='cedalert@brt.it'
047200010911     C                   eval      wrkOgg='VAS - WARNING!!!'
047300010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
047400010615     C                             'SU AS/400'                         +':/N'+
047500010615     C                             ' '                                 +':/N'+
047600010615     C                             'RIFERIMENTI'                       +':/N'+
047700010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
047800010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
047900010615     C                             'Cliente:'+vlrksc                   +':/N'+
048000010615     C                             'TipoFile:'+vlrtip                  +':/N'+
048100010615     C                             'Traduttore:'+vlrpgt                +':/N'+
048200010615     C                             'Parametri:'+vlrppt                 +':/N'+
048300010615     C                             ' '                                 +':/N'+
048400010615     C                             'MESSAGGIO'                         +':/N'+
048500010615     C                             vlrmsg
048600010615     C*
048700010615     C                   call(e)   'TIS701C'
048800010615     C                   parm                    wrkEml
048900010911     C                   parm                    wrkOgg
049000010615     C                   parm                    wrkMsg
049100020118     C*
049200020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
049300080522     C                   if        %lookup(%subst(vlrksc:2:3):skLOG) > *zeros
049400020118     C*
049500120305     C                   eval      wrkEml='cedlog@brt.it'
049600020118     C*
049700020118     C                   call(e)   'TIS701C'
049800020118     C                   parm                    wrkEml
049900020118     C                   parm                    wrkOgg
050000020118     C                   parm                    wrkMsg
050100020118     C*
050200020118     C                   endif
050300020118     C* =====================================================
050400010615     C*
050500010615     C                   endsr
050600051108
050700051108
050800051108
050900051108      /TITLE Reperisce la data ora correnti x indicazione esatta elaborazione
051000051108     C     momentoElab   begsr
051100051108     C*
051200051108     C* Reperimento data corrente
051300051108     C                   time                    wn14             14 0
051400051108     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051500051108     C                   movel     wn14          oracorElab        6 0          *ORA (6) IN HH/MM/SS
051600051108     C                   z-add     wn8           g08dat
051700051108     C                   z-add     *zeros        g08inv
051800051108     C                   movel     '0'           g08err
051900051108     C                   call      'XSRDA8'
052000051108     C                   parm                    wlbda8
052100051108     C                   z-add     g08inv        datcorElab        8 0          *DATA CORRENTE AA/M/
052200051108     C*
052300051108     C* Compongo la data/ora in stringa grosso modo leggibile occupando meno spazio possibile
052400051108     C                   movel     *all'-'       dataoraElab      15
052500051108     C                   movel     datcorElab    dataoraElab
052600051108     C                   move      oracorElab    dataoraElab
052700051108     C*
052800051108     C                   endsr
052900010615
053000010615
053100000626
053200000626
053300990908      /TITLE Operazioni iniziali.
053400990908     C     *inzsr        begsr
053500990908     C*
053600990908     C     *entry        plist
053700990908     C                   parm                    tivlrnrrp
053800000710     C                   parm                    virpgi
053900000710     C                   parm                    virppi
054000000717     C                   parm                    virfit
054100000717     C                   parm                    virfi3
054200000717     C                   parm                    virfi4
054300020107     C*
054400020107     C* Reperimento data corrente
054500020107     C                   time                    wn14             14 0
054600020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054700020107     C                   z-add     wn8           g08dat
054800020107     C                   z-add     *zeros        g08inv
054900020107     C                   movel     '0'           g08err
055000020107     C                   call      'XSRDA8'
055100020107     C                   parm                    wlbda8
055200020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055300080522     C*
055400080522     C* Caricamento filiali network Logistica
055500080522     C                   clear                   skLOG
055600080522     C                   z-add     *zeros        jLOG
055700080522     C     *start        setll     azorg01l
055800080522     C                   read      azorg01l
055900080522     C                   dow       not %eof(azorg01l)
056000080522     C                   eval      og143 = orgDE3
056100080522     C                   if        §OGNTW = 'LOG'
056200080522     C                   add       1             jLOG
056300080522     C                   movel     orgFIL        skLOG(jLOG)
056400080522     C                   endif
056500080522     C                   read      azorg01l
056600080522     C                   enddo
056700990920     C*
056800990920     C     k01vtf01      klist
056900990920     C                   kfld                    vtftip
057000000626     C*
057100000626     C     k03vir02      klist
057200000626     C                   kfld                    virksc
057300000626     C                   kfld                    virtip
057400000626     C                   kfld                    virdti
057500000626     C*
057600000626     C     k02vir02      klist
057700000626     C                   kfld                    virksc
057800000626     C                   kfld                    virtip
057900000626     C*
058000000626     C     k02vss01      klist
058100000626     C                   kfld                    vsssun
058200020107     C                   kfld                    vssisv
058300080526     C*
058400080526     C     KEYvire1_C    klist
058500080526     C                   kfld                    virksc
058600080526     C                   kfld                    virtip
058700080526     C                   kfld                    virdti
058800080526     C                   kfld                    vireTRC
058900990908     C*
059000990908     C                   endsr
