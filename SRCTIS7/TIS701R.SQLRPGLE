000100990910      /TITLE Upload via Internet: decisore.
000200130114     H dftactgrp(*no) actgrp(*caller)
000300990908
000400080522     Fazorg01l  if   e           k disk
000500990908     Ftivlr00f  uf   e             disk
000600990920     Ftivtf01l  if   e           k disk
000700000623     Ftivir02l  if   e           k disk
000800080526     Ftivire1l  if   e           k disk
000900000711     Ftivss01l  if   e           k disk
001000170731     Ftivav01i  uf a e           k disk
001100000711     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001200000627     F                                     PREFIX(f_)
001300990908
001400990910     D dscmz         e ds                  inz
001500080522     D og143         e ds                  inz
001600080526     D dvire01       e ds                  inz
001700080527     D dsvlr3        e ds                  inz
001800170721     D dsvlrpth      e ds                  inz qualified
001900170721     D dvir01        e ds                  inz
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100990920     D tivlrnrrp       s             10s 0
002200021119     D sv_tivlrds    e ds                  extname(tivlr00f) prefix(sv_)
002300030617     D sv2_tivlrds   e ds                  extname(tivlr00f) prefix(sv2_)
002400990921     D prmtivlr        s                   like(tivlrds)
002500990910     D prmesito        s              1
002600000724     D prmlit          s             10    inz(*blanks)
002700000710     D prmfir          s             10    inz(*blanks)
002800021118     D prmfid          s             10    inz(*blanks)
002900051013     D prmmbrfit       s              1    inz(*blanks)
003000080526     D prmtotrcd       s              7  0 inz(*zeros)
003100990910     D wrkesito        s              1
003200000626     D dataiso         s               d
003300000626     D dataiso1        s               d
003400000626     D data8           s              8  0
003500000626     D ora             s              6  0
003600000626     D flgvirfound     s              1    inz('0')
003700000703     D wrkprg          s              8  0 inz(*zeros)
003800000703     D dwlprg          s             10    inz(*all'0')
003900030616     D savMSG          s             30
004000080522     D skLOG           s              3    DIM(200)
004100080522     D jLOG            s              3I 0
004200090407     D conta           s              2  0
004300090407     D contaa          s              2
004400170731
004500170731       // - Status
004600170731     D Psds           sds
004700170731     D   SDSpgm          *proc
004800170731     D   JobName             244    253                                         Job name
004900170731     D   JobUser             254    263                                         User name
005000170731     D   JobNumber           264    269s 0                                      Job number
005100020107
005200020107     D*------------------
005300020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400020107     D*------------------
005500020107     D wlbda8          DS                  INZ
005600020107     D  g08dat                 1      8  0
005700020107     D  g08inv                 9     16  0
005800020107     D  g08err                17     17
005900020107     D  g08tgi                18     22  0
006000020107     D*
006100000626
006200000626
006300090407     C*
006400090407     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006500090407     C
006600090407     C/EXEC SQL
006700090407     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006800090407     C/END-EXEC
006900090407     C*
007000990908     C* Leggo il record di log.
007100990908     C                   dou       %status = 0
007200990908     C     tivlrnrrp     chain(e)  tivlr00f
007300990908     C                   enddo
007400030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
007500030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
007600030613     C                   if        %len(%trim(vlrmsg)) > 0
007700090116     C                   if        %subst(vlrfl3:1:1) = '1'
007800090116     C                   eval      savMSG = %subst(vlrmsg:51)
007900090116     C                   else
008000080522     C                   eval      savMSG = vlrmsg
008100090116     C                   endif
008200030613     C                   endif
008300000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
008400000630     C                   exsr      inzvar
008500000711     C                   exsr      readtivir
008600021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
008700021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
008800021115     C                   eval      vlrpoi = virpoi
008900021115     C                   eval      vlrfl1 = virfi2
009000021115     C                   endif
009100170721     C*
009200170721     C* Se presenti valorizzo FLAG VARI da anagrafica cliente specifica
009300170721     C                   if        virpth <> *blanks
009400170721     C                   eval      dvir01   = virpth
009500170721     C                   eval      dsvlrpth = vlrpth
009600170721     C                   eval      dsvlrpth.§pthVPCO = §vir01VPCO
009700170721     C                   eval      vlrpth = dsvlrpth
009800170721     C                   endif
009900170721     C*
010000010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
010100010710     C* ... o ancora da data-entry giudato via Internet)
010200000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
010300000703     C                             vlrfis = *blanks or
010400011127     C                             vlrfou = *blanks or
010500011127     C                             vlrpgt = *blanks
010600000711     C* Completo i campi mancanti nel record di log
010700000711     C                   eval      vlrlis = '*LIBL'
010800000711     C                   eval      vlrfis = virfis
010900011127     C                   eval      vlrfou = virfit
011000000711     C                   eval      vlrpgt = virpgt
011100000711     C                   eval      vlrppt = virppt
011200000626     C                   else
011300090407     C*
011400090407     C* Verifico elaborabilità transazione (completa)
011500090407     C                   exsr      chkElab
011600090407     C*
011700090407     C* Se transazione (completa) elaborabile => procedo
011800090407     C                   if        *in55 = *on
011900090407     C*
012000000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
012100010524     C                   if        vlrprg = *blanks
012200000627     C                   exsr      calprog
012300000627     C                   endif
012400000626     C                   clear                   wrkesito
012500030616     C***                clear                   vlrmsg
012600030616     C                   move      savMSG        vlrmsg
012700170731     C*
012800170731     C* Prima della traduzione disalloco il record
012900170731     C                   unlock    tivlr00f
013000170731     C*
013100170731     C* Memorizzo il record corrente di TIVLR
013200170731     C                   movel     *blanks       wCurrJob         30
013300170731     C                   eval      wCurrJob = %editc(JobNumber:'X') + '/' +
013400170731     C                                        %trim(JobUser)        + '/' +
013500170731     C                                        %trim(JobName)
013600170731     C                   clear                   tivav000
013700170731     C                   eval      vavKEY1 = 'VASUPL'
013800170731     C                   eval      vavKEY2 = wCurrJob
013900170731     C                   eval      vavKEY3 = 'TIVLR00F_RRN'
014000170731     C                   eval      vavMOM  = %timestamp
014100170731     C                   eval      vavDTA  = %editc(tivlrnrrp:'X')
014200170731     C                   write     tivav000
014300021119     C*
014400990910     C* Traduzione.
014500990910     C                   exsr      traduci
014600170731     C*
014700170731     C* Elimino il record di work relativo al RRN del TIVLR
014800170731     C                   eval      vavKEY1 = 'VASUPL'
014900170731     C                   eval      vavKEY2 = wCurrJob
015000170731     C                   eval      vavKEY3 = 'TIVLR00F_RRN'
015100170731     C     KEYvav01_C    chain     tivav01i
015200170731     C                   if        %found(tivav01i)
015300170731     C                   delete    tivav000
015400170731     C                   endif
015500021119     C*
015600030617     C* Subito dopo la traduzione salvo buffer di ritorno dalla traduzione che può essere cambiato
015700030617     C                   eval      sv2_tivlrds = prmtivlr
015800030617     C* Dopo la traduzione salvo anche il buffer originale del record e rileggo lo stesso record
015900030617     C                   eval      sv_tivlrds = tivlrds
016000021119     C                   dou       %status = 0
016100021119     C     tivlrnrrp     chain(e)  tivlr00f
016200021119     C                   enddo
016300021119     C*
016400030617     C* Dopo aver letto memorizzo gli unici campi che nella traduzione possono essere cambiati
016500030617     C* (vlrfl3 = flag operativi e vlrpoi = P.O. invio file tradotto)
016600030617     C* e re-imposto il buffer salvato prima della rilettura del record.
016700021119     C                   eval      sv_vlrfl3 = vlrfl3
016800021119     C                   eval      tivlrds = sv_tivlrds
016900030617     C                   eval      vlrpoi = sv2_vlrpoi
017000021119     C*
017100000626     C* Imposto stato record.
017200000626     C                   select
017300000626     C                   when      wrkesito = ' '
017400000626     C                   eval      vlrflg = '0'
017500000626     C                   when      wrkesito = '0'
017600000626     C                   eval      vlrflg = '1'
017700000626     C                   other
017800000626     C                   eval      vlrflg = '2'
017900000626     C                   endsl
018000090407     C*
018100090407     C                   endif
018200010615     C*
018300000626     C                   endif
018400021115     C                   exsr      updtivlr
018500000626     C*
018600000627     C                   seton                                        lr
018700090407
018800090407
018900090407
019000000626
019100090407
019200090407      /TITLE Verifica elaborabilità
019300090407     C     chkElab       begsr
019400090407     C*
019500090407     C                   setoff                                       55
019600090407     C*
019700090407     C/EXEC SQL
019800090407     C+ declare C1 cursor for select count(*)
019900090407     C+ from tivlr00f where
020000090407     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020100170817     C+ and vlrsqz = :vlrsqz
020200090421     C+ and vlrflg <> 'M'
020300090407     C+ and vlrpoi <> 0
020400170817     C***+ and ((vlrsqz = :vlrsqz and vlrflg not in ('3', '4'))
020500170817     C***+ or vlrflg='0')
020600090407     C+ for fetch only
020700090407     C/END-EXEC
020800090407     C
020900090407     C/EXEC SQL
021000090407     C+ open C1
021100090407     C/END-EXEC
021200090407     C
021300090407     C/EXEC SQL
021400090407     C+ Fetch C1 into :conta
021500090407     C/END-EXEC
021600090407     C*
021700090407     C                   move      conta         contaa
021800090407     C*
021900090407     C                   if        sqlcod = *zeros and
022000170817     C                             conta  > *zeros and
022100090407     C                             contaa = vlrfl2
022200090407     C                   seton                                        55
022300090407     C                   endif
022400090407     C
022500090407     C/EXEC SQL
022600090407     C+ close C1
022700090407     C/END-EXEC
022800090407     C
022900090407     C*
023000090407     C                   endsr
023100090407
023200090407
023300000627
023400000626
023500000627      /TITLE Valorizzazione Progressivo Applicazione
023600000627     C     calprog       begsr
023700170324     C*
023800170324     C     *start        setll     tis7prgf
023900170324     C                   read(e)   tis7prgf
024000170324     C                   if        not %error
024100000627     C*
024200000627     C                   eval      dwlprg = f_tis7prgf
024300000627     C                   move(p)   dwlprg        wrkprg
024400000627     C                   add       1             wrkprg
024500000627     C                   move(p)   wrkprg        dwlprg
024600000627     C                   movel     vlrisv        dwlprg
024700000627     C*
024800000627     C                   eval      f_tis7prgf = dwlprg
024900010524     C                   update(e) tis7prg0
025000010524     C                   if        not %error
025100010524     C                   eval      vlrprg = dwlprg
025200000711     C                   endif
025300010524     C                   endif
025400000627     C*
025500000627     C                   endsr
025600000627
025700000627
025800000626
025900000626      /TITLE Aggiornamento TIVLR00F.
026000000626     C     updtivlr      begsr
026100000630     C*
026200000626     C* Aggiorno record di log con l'esito dell'elaborazione.
026300000627     C*                  if        vlrflg <> '0'
026400000630     C*
026500051108     C* Nel percorso nome file ricevuto inserisco anche la data/ora d effettiva elaborazione
026600051108     C* (orribilis ma mi serve e qui posso metterlo senza dare fastidio a nulla)
026700051108     C                   exsr      momentoElab
026800170721     C                   eval      dsvlrpth = vlrpth
026900170721     C                   eval      dsvlrpth.§pthDAOR = dataoraElab
027000170721     C                   eval      vlrpth   = dsvlrpth
027100170721     C***                move      dataoraElab   vlrpth
027200051108     C* Quindi aggiorno il record d log corrente
027300990910     C                   update    tivlr000
027400000627     C*                  else
027500000627     C*                  unlock    tivlr00f
027600000627     C*                  endif
027700000626     C*
027800000626     C                   endsr
027900000626
028000000626
028100000623
028200000623      /TITLE Lettura TIVIR00F.
028300000626     C     readtivir     begsr
028400000623     C*
028500000626     C                   clear                   wrkesito
028600000626     C*
028700000626     C                   eval      vsssun = vlrsun
028800020107     C                   eval      vssisv = vlrisv
028900000623     C     k02vss01      chain     tivss01l
029000000623     C*
029100020107     C                   if        not %found or
029200020107     C                             vssdde > datcor or
029300020107     C                             vssdsc < datcor
029400000630     C                   eval      wrkesito='1'
029500000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
029600000630     C                                      'al servizio.'
029700030613     C                   move      savMSG        vlrmsg
029800010615     C                   exsr      sndeml
029900000626     C                   else
030000010710     C*
030100010710     C* Costruisco la parte fissa della chiave
030200010710     C                   eval      virksc = vssksu
030300010710     C                   eval      virtip = vlrtip
030400010710     C                   move      dataiso       dataiso1
030500010710     C                   adddur    1:*d          dataiso1
030600010710     C     *iso          move      dataiso1      virdti
030700010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
030800010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
030900010710     C                   if        vlrtia = 'I'
031000010710     C                   eval      flgvirfound = *on
031100010710     C                   else
031200080522     C                   eval      flgvirfound = *off
031300080522     C                   endif
031400080522     C*
031500000623     C     k03vir02      setll     tivir02l
031600000623     C     k02vir02      readpe    tivir02l
031700000623     C* Se ho letto il record
031800000623     C* ed è valido segnalo che l'ho trovato.
031900000623     C                   if        not %eof
032000000623     C                             and data8 <= virdtf
032100000623     C                   eval      flgvirfound = *on
032200000630     C                   else
032300000630     C                   if        flgvirfound = *off
032400000630     C                   eval      wrkesito='1'
032500000630     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
032600000630     C                                      ' per il cliente.'
032700030613     C                   move      savMSG        vlrmsg
032800010615     C                   exsr      sndeml
032900000623     C                   endif
033000000630     C                   endif
033100010626     C*
033200010626     C* In ogni caso =>
033300010626     C* ... reperisco regole standard.
033400000623     C                   if        flgvirfound = '1'
033500000623     C*
033600010726     C                   eval      vtftip = virtip
033700000623     C     k01vtf01      chain     tivtf01l
033800000623     C*
033900000623     C                   if        %found
034000000623     C*
034100000623     C                   if        virctl = *blanks
034200000623     C                   eval      virctl = vtfctl
034300000623     C                   endif
034400000623     C*
034500000623     C                   if        virpgt = *blanks
034600000623     C                   eval      virpgt = vtfpgt
034700000623     C                   endif
034800000725     C*
034900000725     C                   if        virppt = *blanks
035000000725     C                   eval      virppt = vtfppt
035100000725     C                   endif
035200000713     C*
035300000713     C                   if        virpgi = *blanks
035400000713     C                   eval      virpgi = vtfpgi
035500000713     C                   endif
035600000725     C*
035700101013     C***                if        virppi = *blanks
035800101013     C***                eval      virppi = vtfppi
035900101013     C***                endif
036000000623     C*
036100000623     C                   if        virfid = *blanks
036200000623     C                   eval      virfid = vtffid
036300000623     C                   endif
036400000623     C*
036500000623     C                   if        virfis = *blanks
036600000623     C                   eval      virfis = vtffis
036700000623     C                   endif
036800000623     C*
036900000623     C                   if        virfit = *blanks
037000000623     C                   eval      virfit = vtffit
037100000623     C                   endif
037200051013     C*
037300051013     C                   eval      prmmbrfit = %subst(vtffi2:1:1)
037400000623     C*
037500000630     C                   else
037600000630     C                   eval      wrkesito='1'
037700000630     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
037800030613     C                   move      savMSG        vlrmsg
037900010615     C                   exsr      sndeml
038000000623     C                   endif
038100000623     C*
038200000623     C                   endif
038300000626     C                   endif
038400000623     C*
038500000710     C                   if        wrkesito<>*blanks
038600000630     C                   eval      vlrflg = '2'
038700000630     C                   endif
038800000630     C*
038900000623     C                   endsr
039000000711
039100000711
039200000711
039300990910      /TITLE Traduzione.
039400990910     C     traduci       begsr
039500990910     C* Chiamo il pgm traduttore.
039600990920     C                   call(e)   'TIS710C'
039700990920     C                   parm      tivlrds       prmtivlr
039800990910     C                   parm      *blanks       prmesito
039900000724     C                   parm      virfi3        prmlit
040000000710     C                   parm      virfi4        prmfir
040100021118     C                   parm      virfid        prmfid
040200051013     C                   parm                    prmmbrfit
040300080526     C                   parm                    prmtotrcd
040400990910     C*
040500990910     C                   select
040600000725     C* Se la call è finita in errore
040700000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
040800000725     C                   when      %error
040900000725     C                   eval      wrkesito = '1'
041000000725     C                   eval      vlrmsg = 'Errori non identificati' +
041100000725     C                                      ' in fase di trad.'
041200030613     C                   move      savMSG        vlrmsg
041300010615     C                   exsr      sndeml
041400000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
041500000725     C                   when      prmesito = '1'
041600990910     C                   eval      wrkesito = '1'
041700000710     C                   eval      vlrmsg = 'Invio effettuato,' +
041800000710     C                                      ' ma errori nella trad.'
041900030613     C                   move      savMSG        vlrmsg
042000000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
042100000725     C                   when      prmesito = '2'
042200000710     C                   eval      wrkesito = '1'
042300010615     C                   eval      vlrmsg = 'Errori in trad. ed' +
042400010615     C                                      ' invio non effettuato'
042500030613     C                   move      savMSG        vlrmsg
042600010615     C                   exsr      sndeml
042700170519     C* Se la sottomissione dell'invio al PO non è riuscita OPPURE in fase di applicazione
042800170519     C* personalizzazioni cliente previste in tabella VPC.
042900000725     C                   when      prmesito = '3'
043000990921     C                   eval      wrkesito = '1'
043100170519     C                   eval      vlrmsg = 'Errori invio a FIL ' +
043200170519     C                                      'o personalizz. VPC'
043300030613     C                   move      savMSG        vlrmsg
043400010615     C                   exsr      sndeml
043500010615     C* Se la il pgm traduttore non esiste.
043600010615     C                   when      prmesito = '4'
043700010615     C                   eval      wrkesito = '1'
043800010615     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
043900030613     C                   move      savMSG        vlrmsg
044000010615     C                   exsr      sndeml
044100010615     C* Se la chiamata al pgm traduttore fallisce.
044200010615     C                   when      prmesito = '5'
044300010615     C                   eval      wrkesito = '1'
044400010618     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
044500030613     C                   move      savMSG        vlrmsg
044600010615     C                   exsr      sndeml
044700010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
044800010615     C                   when      prmesito = '6'
044900010615     C                   eval      wrkesito = '1'
045000010618     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
045100010618     C                                      'invio effettuato ma verif.'
045200030613     C                   move      savMSG        vlrmsg
045300010615     C                   exsr      sndeml
045400021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
045500021118     C                   when      prmesito = '7'
045600021118     C                   eval      wrkesito = '1'
045700021118     C                   eval      vlrmsg = 'Errori in storicizzazione '
045800030613     C                   move      savMSG        vlrmsg
045900021118     C                   exsr      sndeml
046000030710     C* Se la traduzione di EDIVAB/EDIVAD ha avuto degli errori (occorre verificare).
046100030710     C                   when      prmesito = '8'
046200030710     C                   eval      wrkesito = '1'
046300030710     C                   eval      vlrmsg = 'Errore, nome file ' +
046400030710     C                                      'o parametri mancanti.'
046500030710     C                   move      savMSG        vlrmsg
046600030710     C                   exsr      sndeml
046700000725     C* Se la traduzione è stata eseguita senza errori.
046800990910     C                   when      not %error and prmesito = '0'
046900990910     C                   eval      wrkesito = '0'
047000000725     C                   eval      vlrmsg = 'Upload eseguito con successo.'
047100030613     C                   move      savMSG        vlrmsg
047200990910     C                   endsl
047300080526     C*
047400080526     C* Verifico in anagrafica clienti/tipi-file se richiesto l'invio della mail
047500080527     C* conferma ricezione file BARTVAS (solo se NN riesecuzione da log)
047600080527     C                   eval      dsvlr3 = vlrFL3
047700080527     C                   if        §VLR31 = *blanks
047800080526     C                   eval      vireTRC = '01'
047900080526     C     KEYvire1_C    chain     tivire1l
048000080526     C                   if        %found(tivire1l)
048100080526     C                   eval      dvire01 = vireFLO
048200080526     C                   if        §VIRE01INV = 'S'
048300080526     C                   call      'TIS7EMLR4'
048400080526     C                   parm                    virKSC
048500080527     C                   parm                    savMSG
048600080526     C                   parm                    prmtotrcd
048700080526     C                   parm                    §VIRE01EML
048800080526     C                   endif
048900080527     C                   endif
049000080526     C                   endif
049100990910     C*
049200990910     C                   endsr
049300990910
049400000626
049500000626
049600000626      /TITLE Inizializzazioni variabili.
049700000626     C     inzvar        begsr
049800000626     C*
049900000626     C                   time                    dataiso
050000000626     C     *iso          move      dataiso       data8
050100000626     C                   time                    ora
050200000626     C*
050300000626     C                   endsr
050400000626
050500010615
050600010615
050700010615
050800010615      /TITLE Compongo il testo e spedisco una e-m@ail
050900010615     C     sndeml        begsr
051000010615     C*
051100010615     C* Inizializzo variabili
051200010911     C                   movel     *blanks       wrkEml          253
051300010911     C                   movel     *blanks       wrkMsg         5000
051400010911     C                   movel     *blanks       wrkOgg           44
051500010615     C* Valorizzo i campi della e-m@ail
051600120305     C                   eval      wrkEml='cedalert@brt.it'
051700010911     C                   eval      wrkOgg='VAS - WARNING!!!'
051800010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
051900010615     C                             'SU AS/400'                         +':/N'+
052000010615     C                             ' '                                 +':/N'+
052100010615     C                             'RIFERIMENTI'                       +':/N'+
052200010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
052300010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
052400010615     C                             'Cliente:'+vlrksc                   +':/N'+
052500010615     C                             'TipoFile:'+vlrtip                  +':/N'+
052600010615     C                             'Traduttore:'+vlrpgt                +':/N'+
052700010615     C                             'Parametri:'+vlrppt                 +':/N'+
052800010615     C                             ' '                                 +':/N'+
052900010615     C                             'MESSAGGIO'                         +':/N'+
053000010615     C                             vlrmsg
053100010615     C*
053200010615     C                   call(e)   'TIS701C'
053300010615     C                   parm                    wrkEml
053400010911     C                   parm                    wrkOgg
053500010615     C                   parm                    wrkMsg
053600020118     C*
053700020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
053800080522     C                   if        %lookup(%subst(vlrksc:2:3):skLOG) > *zeros
053900020118     C*
054000120305     C                   eval      wrkEml='cedlog@brt.it'
054100020118     C*
054200020118     C                   call(e)   'TIS701C'
054300020118     C                   parm                    wrkEml
054400020118     C                   parm                    wrkOgg
054500020118     C                   parm                    wrkMsg
054600020118     C*
054700020118     C                   endif
054800020118     C* =====================================================
054900010615     C*
055000010615     C                   endsr
055100051108
055200051108
055300051108
055400051108      /TITLE Reperisce la data ora correnti x indicazione esatta elaborazione
055500051108     C     momentoElab   begsr
055600051108     C*
055700051108     C* Reperimento data corrente
055800051108     C                   time                    wn14             14 0
055900051108     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056000051108     C                   movel     wn14          oracorElab        6 0          *ORA (6) IN HH/MM/SS
056100051108     C                   z-add     wn8           g08dat
056200051108     C                   z-add     *zeros        g08inv
056300051108     C                   movel     '0'           g08err
056400051108     C                   call      'XSRDA8'
056500051108     C                   parm                    wlbda8
056600051108     C                   z-add     g08inv        datcorElab        8 0          *DATA CORRENTE AA/M/
056700051108     C*
056800051108     C* Compongo la data/ora in stringa grosso modo leggibile occupando meno spazio possibile
056900051108     C                   movel     *all'-'       dataoraElab      15
057000051108     C                   movel     datcorElab    dataoraElab
057100051108     C                   move      oracorElab    dataoraElab
057200051108     C*
057300051108     C                   endsr
057400010615
057500010615
057600000626
057700000626
057800990908      /TITLE Operazioni iniziali.
057900990908     C     *inzsr        begsr
058000990908     C*
058100990908     C     *entry        plist
058200990908     C                   parm                    tivlrnrrp
058300000710     C                   parm                    virpgi
058400000710     C                   parm                    virppi
058500000717     C                   parm                    virfit
058600000717     C                   parm                    virfi3
058700000717     C                   parm                    virfi4
058800020107     C*
058900020107     C* Reperimento data corrente
059000020107     C                   time                    wn14             14 0
059100020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059200020107     C                   z-add     wn8           g08dat
059300020107     C                   z-add     *zeros        g08inv
059400020107     C                   movel     '0'           g08err
059500020107     C                   call      'XSRDA8'
059600020107     C                   parm                    wlbda8
059700020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059800080522     C*
059900080522     C* Caricamento filiali network Logistica
060000080522     C                   clear                   skLOG
060100080522     C                   z-add     *zeros        jLOG
060200080522     C     *start        setll     azorg01l
060300080522     C                   read      azorg01l
060400080522     C                   dow       not %eof(azorg01l)
060500080522     C                   eval      og143 = orgDE3
060600080522     C                   if        §OGNTW = 'LOG'
060700080522     C                   add       1             jLOG
060800080522     C                   movel     orgFIL        skLOG(jLOG)
060900080522     C                   endif
061000080522     C                   read      azorg01l
061100080522     C                   enddo
061200990920     C*
061300990920     C     k01vtf01      klist
061400990920     C                   kfld                    vtftip
061500000626     C*
061600000626     C     k03vir02      klist
061700000626     C                   kfld                    virksc
061800000626     C                   kfld                    virtip
061900000626     C                   kfld                    virdti
062000000626     C*
062100000626     C     k02vir02      klist
062200000626     C                   kfld                    virksc
062300000626     C                   kfld                    virtip
062400000626     C*
062500000626     C     k02vss01      klist
062600000626     C                   kfld                    vsssun
062700020107     C                   kfld                    vssisv
062800080526     C*
062900080526     C     KEYvire1_C    klist
063000080526     C                   kfld                    virksc
063100080526     C                   kfld                    virtip
063200080526     C                   kfld                    virdti
063300080526     C                   kfld                    vireTRC
063400170731     C*
063500170731     C     KEYvav01_C    klist
063600170731     C                   kfld                    vavKEY1
063700170731     C                   kfld                    vavKEY2
063800170731     C                   kfld                    vavKEY3
063900990908     C*
064000990908     C                   endsr
