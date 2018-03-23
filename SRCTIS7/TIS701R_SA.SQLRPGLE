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
001100170731     Ftivav01i  uf a e           k disk
001200000711     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001300000627     F                                     PREFIX(f_)
001400990908
001500990910     D dscmz         e ds                  inz
001600080522     D og143         e ds                  inz
001700080526     D dvire01       e ds                  inz
001800080527     D dsvlr3        e ds                  inz
001900170721     D dsvlrpth      e ds                  inz qualified
002000170721     D dvir01        e ds                  inz
002100990920     D tivlrds       e ds                  extname(tivlr00f)
002200990920     D tivlrnrrp       s             10s 0
002300021119     D sv_tivlrds    e ds                  extname(tivlr00f) prefix(sv_)
002400030617     D sv2_tivlrds   e ds                  extname(tivlr00f) prefix(sv2_)
002500990921     D prmtivlr        s                   like(tivlrds)
002600990910     D prmesito        s              1
002700000724     D prmlit          s             10    inz(*blanks)
002800000710     D prmfir          s             10    inz(*blanks)
002900021118     D prmfid          s             10    inz(*blanks)
003000051013     D prmmbrfit       s              1    inz(*blanks)
003100080526     D prmtotrcd       s              7  0 inz(*zeros)
003200990910     D wrkesito        s              1
003300000626     D dataiso         s               d
003400000626     D dataiso1        s               d
003500000626     D data8           s              8  0
003600000626     D ora             s              6  0
003700000626     D flgvirfound     s              1    inz('0')
003800000703     D wrkprg          s              8  0 inz(*zeros)
003900000703     D dwlprg          s             10    inz(*all'0')
004000030616     D savMSG          s             30
004100080522     D skLOG           s              3    DIM(200)
004200080522     D jLOG            s              3I 0
004300090407     D conta           s              2  0
004400090407     D contaa          s              2
004500170731
004600170731       // - Status
004700170731     D Psds           sds
004800170731     D   SDSpgm          *proc
004900170731     D   JobName             244    253                                         Job name
005000170731     D   JobUser             254    263                                         User name
005100170731     D   JobNumber           264    269s 0                                      Job number
005200020107
005300020107     D*------------------
005400020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500020107     D*------------------
005600020107     D wlbda8          DS                  INZ
005700020107     D  g08dat                 1      8  0
005800020107     D  g08inv                 9     16  0
005900020107     D  g08err                17     17
006000020107     D  g08tgi                18     22  0
006100020107     D*
006200000626
006300000626
006400090407     C*
006500090407     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006600090407     C
006700090407     C/EXEC SQL
006800090407     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006900090407     C/END-EXEC
007000090407     C*
007100990908     C* Leggo il record di log.
007200990908     C                   dou       %status = 0
007300990908     C     tivlrnrrp     chain(e)  tivlr00f
007400990908     C                   enddo
007500030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
007600030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
007700030613     C                   if        %len(%trim(vlrmsg)) > 0
007800090116     C                   if        %subst(vlrfl3:1:1) = '1'
007900090116     C                   eval      savMSG = %subst(vlrmsg:51)
008000090116     C                   else
008100080522     C                   eval      savMSG = vlrmsg
008200090116     C                   endif
008300030613     C                   endif
008400000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
008500000630     C                   exsr      inzvar
008600000711     C                   exsr      readtivir
008700021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
008800021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
008900021115     C                   eval      vlrpoi = virpoi
009000021115     C                   eval      vlrfl1 = virfi2
009100021115     C                   endif
009200170721     C*
009300170721     C* Se presenti valorizzo FLAG VARI da anagrafica cliente specifica
009400170721     C                   if        virpth <> *blanks
009500170721     C                   eval      dvir01   = virpth
009600170721     C                   eval      dsvlrpth = vlrpth
009700170721     C                   eval      dsvlrpth.§pthVPCO = §vir01VPCO
009800170721     C                   eval      vlrpth = dsvlrpth
009900170721     C                   endif
010000170721     C*
010100010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
010200010710     C* ... o ancora da data-entry giudato via Internet)
010300000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
010400000703     C                             vlrfis = *blanks or
010500011127     C                             vlrfou = *blanks or
010600011127     C                             vlrpgt = *blanks
010700000711     C* Completo i campi mancanti nel record di log
010800000711     C                   eval      vlrlis = '*LIBL'
010900000711     C                   eval      vlrfis = virfis
011000011127     C                   eval      vlrfou = virfit
011100000711     C                   eval      vlrpgt = virpgt
011200000711     C                   eval      vlrppt = virppt
011300000626     C                   else
011400090407     C*
011500090407     C* Verifico elaborabilità transazione (completa)
011600090407     C                   exsr      chkElab
011700090407     C*
011800090407     C* Se transazione (completa) elaborabile => procedo
011900090407     C                   if        *in55 = *on
012000090407     C*
012100000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
012200010524     C                   if        vlrprg = *blanks
012300000627     C                   exsr      calprog
012400000627     C                   endif
012500000626     C                   clear                   wrkesito
012600030616     C***                clear                   vlrmsg
012700030616     C                   move      savMSG        vlrmsg
012800170731     C*
012900170731     C* Prima della traduzione disalloco il record
013000170731     C                   unlock    tivlr00f
013100170731     C*
013200170731     C* Memorizzo il record corrente di TIVLR
013300170731     C                   movel     *blanks       wCurrJob         30
013400170731     C                   eval      wCurrJob = %editc(JobNumber:'X') + '/' +
013500170731     C                                        %trim(JobUser)        + '/' +
013600170731     C                                        %trim(JobName)
013700170731     C                   clear                   tivav000
013800170731     C                   eval      vavKEY1 = 'VASUPL'
013900170731     C                   eval      vavKEY2 = wCurrJob
014000170731     C                   eval      vavKEY3 = 'TIVLR00F_RRN'
014100170731     C                   eval      vavMOM  = %timestamp
014200170731     C                   eval      vavDTA  = %editc(tivlrnrrp:'X')
014300170731     C                   write     tivav000
014400021119     C*
014500990910     C* Traduzione.
014600990910     C                   exsr      traduci
014700170731     C*
014800170731     C* Memorizzo il record corrente di TIVLR
014900170731     C                   eval      vavKEY1 = 'VASUPL'
015000170731     C                   eval      vavKEY2 = wCurrJob
015100170731     C                   eval      vavKEY3 = 'TIVLR00F_RRN'
015200170731     C     KEYvav01_C    chain     tivav01i
015300170731     C                   if        %found(tivav01i)
015400170731     C                   delete    tivav000
015500170731     C                   endif
015600021119     C*
015700030617     C* Subito dopo la traduzione salvo buffer di ritorno dalla traduzione che può essere cambiato
015800030617     C                   eval      sv2_tivlrds = prmtivlr
015900030617     C* Dopo la traduzione salvo anche il buffer originale del record e rileggo lo stesso record
016000030617     C                   eval      sv_tivlrds = tivlrds
016100021119     C                   dou       %status = 0
016200021119     C     tivlrnrrp     chain(e)  tivlr00f
016300021119     C                   enddo
016400021119     C*
016500030617     C* Dopo aver letto memorizzo gli unici campi che nella traduzione possono essere cambiati
016600030617     C* (vlrfl3 = flag operativi e vlrpoi = P.O. invio file tradotto)
016700030617     C* e re-imposto il buffer salvato prima della rilettura del record.
016800021119     C                   eval      sv_vlrfl3 = vlrfl3
016900021119     C                   eval      tivlrds = sv_tivlrds
017000030617     C                   eval      vlrpoi = sv2_vlrpoi
017100021119     C*
017200000626     C* Imposto stato record.
017300000626     C                   select
017400000626     C                   when      wrkesito = ' '
017500000626     C                   eval      vlrflg = '0'
017600000626     C                   when      wrkesito = '0'
017700000626     C                   eval      vlrflg = '1'
017800000626     C                   other
017900000626     C                   eval      vlrflg = '2'
018000000626     C                   endsl
018100090407     C*
018200090407     C                   endif
018300010615     C*
018400000626     C                   endif
018500021115     C                   exsr      updtivlr
018600000626     C*
018700000627     C                   seton                                        lr
018800090407
018900090407
019000090407
019100000626
019200090407
019300090407      /TITLE Verifica elaborabilità
019400090407     C     chkElab       begsr
019500090407     C*
019600090407     C                   setoff                                       55
019700090407     C*
019800090407     C/EXEC SQL
019900090407     C+ declare C1 cursor for select count(*)
020000090407     C+ from tivlr00f where
020100090407     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020200090421     C+ and vlrflg <> 'M'
020300090407     C+ and vlrpoi <> 0
020400090504     C+ and ((vlrsqz = :vlrsqz and vlrflg not in ('3', '4'))
020500090504     C+ or vlrflg='0')
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
022000090407     C                             contaa = vlrfl2
022100090407     C                   seton                                        55
022200090407     C                   endif
022300090407     C
022400090407     C/EXEC SQL
022500090407     C+ close C1
022600090407     C/END-EXEC
022700090407     C
022800090407     C*
022900090407     C                   endsr
023000090407
023100090407
023200000627
023300000626
023400000627      /TITLE Valorizzazione Progressivo Applicazione
023500000627     C     calprog       begsr
023600170324     C*
023700170324     C     *start        setll     tis7prgf
023800170324     C                   read(e)   tis7prgf
023900170324     C                   if        not %error
024000000627     C*
024100000627     C                   eval      dwlprg = f_tis7prgf
024200000627     C                   move(p)   dwlprg        wrkprg
024300000627     C                   add       1             wrkprg
024400000627     C                   move(p)   wrkprg        dwlprg
024500000627     C                   movel     vlrisv        dwlprg
024600000627     C*
024700000627     C                   eval      f_tis7prgf = dwlprg
024800010524     C                   update(e) tis7prg0
024900010524     C                   if        not %error
025000010524     C                   eval      vlrprg = dwlprg
025100000711     C                   endif
025200010524     C                   endif
025300000627     C*
025400000627     C                   endsr
025500000627
025600000627
025700000626
025800000626      /TITLE Aggiornamento TIVLR00F.
025900000626     C     updtivlr      begsr
026000000630     C*
026100000626     C* Aggiorno record di log con l'esito dell'elaborazione.
026200000627     C*                  if        vlrflg <> '0'
026300000630     C*
026400051108     C* Nel percorso nome file ricevuto inserisco anche la data/ora d effettiva elaborazione
026500051108     C* (orribilis ma mi serve e qui posso metterlo senza dare fastidio a nulla)
026600051108     C                   exsr      momentoElab
026700170721     C                   eval      dsvlrpth = vlrpth
026800170721     C                   eval      dsvlrpth.§pthDAOR = dataoraElab
026900170721     C                   eval      vlrpth   = dsvlrpth
027000170721     C***                move      dataoraElab   vlrpth
027100051108     C* Quindi aggiorno il record d log corrente
027200990910     C                   update    tivlr000
027300000627     C*                  else
027400000627     C*                  unlock    tivlr00f
027500000627     C*                  endif
027600000626     C*
027700000626     C                   endsr
027800000626
027900000626
028000000623
028100000623      /TITLE Lettura TIVIR00F.
028200000626     C     readtivir     begsr
028300000623     C*
028400000626     C                   clear                   wrkesito
028500000626     C*
028600000626     C                   eval      vsssun = vlrsun
028700020107     C                   eval      vssisv = vlrisv
028800000623     C     k02vss01      chain     tivss01l
028900000623     C*
029000020107     C                   if        not %found or
029100020107     C                             vssdde > datcor or
029200020107     C                             vssdsc < datcor
029300000630     C                   eval      wrkesito='1'
029400000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
029500000630     C                                      'al servizio.'
029600030613     C                   move      savMSG        vlrmsg
029700010615     C                   exsr      sndeml
029800000626     C                   else
029900010710     C*
030000010710     C* Costruisco la parte fissa della chiave
030100010710     C                   eval      virksc = vssksu
030200010710     C                   eval      virtip = vlrtip
030300010710     C                   move      dataiso       dataiso1
030400010710     C                   adddur    1:*d          dataiso1
030500010710     C     *iso          move      dataiso1      virdti
030600010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
030700010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
030800010710     C                   if        vlrtia = 'I'
030900010710     C                   eval      flgvirfound = *on
031000010710     C                   else
031100080522     C                   eval      flgvirfound = *off
031200080522     C                   endif
031300080522     C*
031400000623     C     k03vir02      setll     tivir02l
031500000623     C     k02vir02      readpe    tivir02l
031600000623     C* Se ho letto il record
031700000623     C* ed è valido segnalo che l'ho trovato.
031800000623     C                   if        not %eof
031900000623     C                             and data8 <= virdtf
032000000623     C                   eval      flgvirfound = *on
032100000630     C                   else
032200000630     C                   if        flgvirfound = *off
032300000630     C                   eval      wrkesito='1'
032400000630     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
032500000630     C                                      ' per il cliente.'
032600030613     C                   move      savMSG        vlrmsg
032700010615     C                   exsr      sndeml
032800000623     C                   endif
032900000630     C                   endif
033000010626     C*
033100010626     C* In ogni caso =>
033200010626     C* ... reperisco regole standard.
033300000623     C                   if        flgvirfound = '1'
033400000623     C*
033500010726     C                   eval      vtftip = virtip
033600000623     C     k01vtf01      chain     tivtf01l
033700000623     C*
033800000623     C                   if        %found
033900000623     C*
034000000623     C                   if        virctl = *blanks
034100000623     C                   eval      virctl = vtfctl
034200000623     C                   endif
034300000623     C*
034400000623     C                   if        virpgt = *blanks
034500000623     C                   eval      virpgt = vtfpgt
034600000623     C                   endif
034700000725     C*
034800000725     C                   if        virppt = *blanks
034900000725     C                   eval      virppt = vtfppt
035000000725     C                   endif
035100000713     C*
035200000713     C                   if        virpgi = *blanks
035300000713     C                   eval      virpgi = vtfpgi
035400000713     C                   endif
035500000725     C*
035600101013     C***                if        virppi = *blanks
035700101013     C***                eval      virppi = vtfppi
035800101013     C***                endif
035900000623     C*
036000000623     C                   if        virfid = *blanks
036100000623     C                   eval      virfid = vtffid
036200000623     C                   endif
036300000623     C*
036400000623     C                   if        virfis = *blanks
036500000623     C                   eval      virfis = vtffis
036600000623     C                   endif
036700000623     C*
036800000623     C                   if        virfit = *blanks
036900000623     C                   eval      virfit = vtffit
037000000623     C                   endif
037100051013     C*
037200051013     C                   eval      prmmbrfit = %subst(vtffi2:1:1)
037300000623     C*
037400000630     C                   else
037500000630     C                   eval      wrkesito='1'
037600000630     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
037700030613     C                   move      savMSG        vlrmsg
037800010615     C                   exsr      sndeml
037900000623     C                   endif
038000000623     C*
038100000623     C                   endif
038200000626     C                   endif
038300000623     C*
038400000710     C                   if        wrkesito<>*blanks
038500000630     C                   eval      vlrflg = '2'
038600000630     C                   endif
038700000630     C*
038800000623     C                   endsr
038900000711
039000000711
039100000711
039200990910      /TITLE Traduzione.
039300990910     C     traduci       begsr
039400990910     C* Chiamo il pgm traduttore.
039500990920     C                   call(e)   'TIS710C'
039600990920     C                   parm      tivlrds       prmtivlr
039700990910     C                   parm      *blanks       prmesito
039800000724     C                   parm      virfi3        prmlit
039900000710     C                   parm      virfi4        prmfir
040000021118     C                   parm      virfid        prmfid
040100051013     C                   parm                    prmmbrfit
040200080526     C                   parm                    prmtotrcd
040300990910     C*
040400990910     C                   select
040500000725     C* Se la call è finita in errore
040600000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
040700000725     C                   when      %error
040800000725     C                   eval      wrkesito = '1'
040900000725     C                   eval      vlrmsg = 'Errori non identificati' +
041000000725     C                                      ' in fase di trad.'
041100030613     C                   move      savMSG        vlrmsg
041200010615     C                   exsr      sndeml
041300000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
041400000725     C                   when      prmesito = '1'
041500990910     C                   eval      wrkesito = '1'
041600000710     C                   eval      vlrmsg = 'Invio effettuato,' +
041700000710     C                                      ' ma errori nella trad.'
041800030613     C                   move      savMSG        vlrmsg
041900000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
042000000725     C                   when      prmesito = '2'
042100000710     C                   eval      wrkesito = '1'
042200010615     C                   eval      vlrmsg = 'Errori in trad. ed' +
042300010615     C                                      ' invio non effettuato'
042400030613     C                   move      savMSG        vlrmsg
042500010615     C                   exsr      sndeml
042600170519     C* Se la sottomissione dell'invio al PO non è riuscita OPPURE in fase di applicazione
042700170519     C* personalizzazioni cliente previste in tabella VPC.
042800000725     C                   when      prmesito = '3'
042900990921     C                   eval      wrkesito = '1'
043000170519     C                   eval      vlrmsg = 'Errori invio a FIL ' +
043100170519     C                                      'o personalizz. VPC'
043200030613     C                   move      savMSG        vlrmsg
043300010615     C                   exsr      sndeml
043400010615     C* Se la il pgm traduttore non esiste.
043500010615     C                   when      prmesito = '4'
043600010615     C                   eval      wrkesito = '1'
043700010615     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
043800030613     C                   move      savMSG        vlrmsg
043900010615     C                   exsr      sndeml
044000010615     C* Se la chiamata al pgm traduttore fallisce.
044100010615     C                   when      prmesito = '5'
044200010615     C                   eval      wrkesito = '1'
044300010618     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
044400030613     C                   move      savMSG        vlrmsg
044500010615     C                   exsr      sndeml
044600010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
044700010615     C                   when      prmesito = '6'
044800010615     C                   eval      wrkesito = '1'
044900010618     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
045000010618     C                                      'invio effettuato ma verif.'
045100030613     C                   move      savMSG        vlrmsg
045200010615     C                   exsr      sndeml
045300021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
045400021118     C                   when      prmesito = '7'
045500021118     C                   eval      wrkesito = '1'
045600021118     C                   eval      vlrmsg = 'Errori in storicizzazione '
045700030613     C                   move      savMSG        vlrmsg
045800021118     C                   exsr      sndeml
045900030710     C* Se la traduzione di EDIVAB/EDIVAD ha avuto degli errori (occorre verificare).
046000030710     C                   when      prmesito = '8'
046100030710     C                   eval      wrkesito = '1'
046200030710     C                   eval      vlrmsg = 'Errore, nome file ' +
046300030710     C                                      'o parametri mancanti.'
046400030710     C                   move      savMSG        vlrmsg
046500030710     C                   exsr      sndeml
046600000725     C* Se la traduzione è stata eseguita senza errori.
046700990910     C                   when      not %error and prmesito = '0'
046800990910     C                   eval      wrkesito = '0'
046900000725     C                   eval      vlrmsg = 'Upload eseguito con successo.'
047000030613     C                   move      savMSG        vlrmsg
047100990910     C                   endsl
047200080526     C*
047300080526     C* Verifico in anagrafica clienti/tipi-file se richiesto l'invio della mail
047400080527     C* conferma ricezione file BARTVAS (solo se NN riesecuzione da log)
047500080527     C                   eval      dsvlr3 = vlrFL3
047600080527     C                   if        §VLR31 = *blanks
047700080526     C                   eval      vireTRC = '01'
047800080526     C     KEYvire1_C    chain     tivire1l
047900080526     C                   if        %found(tivire1l)
048000080526     C                   eval      dvire01 = vireFLO
048100080526     C                   if        §VIRE01INV = 'S'
048200080526     C                   call      'TIS7EMLR4'
048300080526     C                   parm                    virKSC
048400080527     C                   parm                    savMSG
048500080526     C                   parm                    prmtotrcd
048600080526     C                   parm                    §VIRE01EML
048700080526     C                   endif
048800080527     C                   endif
048900080526     C                   endif
049000990910     C*
049100990910     C                   endsr
049200990910
049300000626
049400000626
049500000626      /TITLE Inizializzazioni variabili.
049600000626     C     inzvar        begsr
049700000626     C*
049800000626     C                   time                    dataiso
049900000626     C     *iso          move      dataiso       data8
050000000626     C                   time                    ora
050100000626     C*
050200000626     C                   endsr
050300000626
050400010615
050500010615
050600010615
050700010615      /TITLE Compongo il testo e spedisco una e-m@ail
050800010615     C     sndeml        begsr
050900010615     C*
051000010615     C* Inizializzo variabili
051100010911     C                   movel     *blanks       wrkEml          253
051200010911     C                   movel     *blanks       wrkMsg         5000
051300010911     C                   movel     *blanks       wrkOgg           44
051400010615     C* Valorizzo i campi della e-m@ail
051500120305     C                   eval      wrkEml='cedalert@brt.it'
051600010911     C                   eval      wrkOgg='VAS - WARNING!!!'
051700010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
051800010615     C                             'SU AS/400'                         +':/N'+
051900010615     C                             ' '                                 +':/N'+
052000010615     C                             'RIFERIMENTI'                       +':/N'+
052100010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
052200010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
052300010615     C                             'Cliente:'+vlrksc                   +':/N'+
052400010615     C                             'TipoFile:'+vlrtip                  +':/N'+
052500010615     C                             'Traduttore:'+vlrpgt                +':/N'+
052600010615     C                             'Parametri:'+vlrppt                 +':/N'+
052700010615     C                             ' '                                 +':/N'+
052800010615     C                             'MESSAGGIO'                         +':/N'+
052900010615     C                             vlrmsg
053000010615     C*
053100010615     C                   call(e)   'TIS701C'
053200010615     C                   parm                    wrkEml
053300010911     C                   parm                    wrkOgg
053400010615     C                   parm                    wrkMsg
053500020118     C*
053600020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
053700080522     C                   if        %lookup(%subst(vlrksc:2:3):skLOG) > *zeros
053800020118     C*
053900120305     C                   eval      wrkEml='cedlog@brt.it'
054000020118     C*
054100020118     C                   call(e)   'TIS701C'
054200020118     C                   parm                    wrkEml
054300020118     C                   parm                    wrkOgg
054400020118     C                   parm                    wrkMsg
054500020118     C*
054600020118     C                   endif
054700020118     C* =====================================================
054800010615     C*
054900010615     C                   endsr
055000051108
055100051108
055200051108
055300051108      /TITLE Reperisce la data ora correnti x indicazione esatta elaborazione
055400051108     C     momentoElab   begsr
055500051108     C*
055600051108     C* Reperimento data corrente
055700051108     C                   time                    wn14             14 0
055800051108     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055900051108     C                   movel     wn14          oracorElab        6 0          *ORA (6) IN HH/MM/SS
056000051108     C                   z-add     wn8           g08dat
056100051108     C                   z-add     *zeros        g08inv
056200051108     C                   movel     '0'           g08err
056300051108     C                   call      'XSRDA8'
056400051108     C                   parm                    wlbda8
056500051108     C                   z-add     g08inv        datcorElab        8 0          *DATA CORRENTE AA/M/
056600051108     C*
056700051108     C* Compongo la data/ora in stringa grosso modo leggibile occupando meno spazio possibile
056800051108     C                   movel     *all'-'       dataoraElab      15
056900051108     C                   movel     datcorElab    dataoraElab
057000051108     C                   move      oracorElab    dataoraElab
057100051108     C*
057200051108     C                   endsr
057300010615
057400010615
057500000626
057600000626
057700990908      /TITLE Operazioni iniziali.
057800990908     C     *inzsr        begsr
057900990908     C*
058000990908     C     *entry        plist
058100990908     C                   parm                    tivlrnrrp
058200000710     C                   parm                    virpgi
058300000710     C                   parm                    virppi
058400000717     C                   parm                    virfit
058500000717     C                   parm                    virfi3
058600000717     C                   parm                    virfi4
058700020107     C*
058800020107     C* Reperimento data corrente
058900020107     C                   time                    wn14             14 0
059000020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059100020107     C                   z-add     wn8           g08dat
059200020107     C                   z-add     *zeros        g08inv
059300020107     C                   movel     '0'           g08err
059400020107     C                   call      'XSRDA8'
059500020107     C                   parm                    wlbda8
059600020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059700080522     C*
059800080522     C* Caricamento filiali network Logistica
059900080522     C                   clear                   skLOG
060000080522     C                   z-add     *zeros        jLOG
060100080522     C     *start        setll     azorg01l
060200080522     C                   read      azorg01l
060300080522     C                   dow       not %eof(azorg01l)
060400080522     C                   eval      og143 = orgDE3
060500080522     C                   if        §OGNTW = 'LOG'
060600080522     C                   add       1             jLOG
060700080522     C                   movel     orgFIL        skLOG(jLOG)
060800080522     C                   endif
060900080522     C                   read      azorg01l
061000080522     C                   enddo
061100990920     C*
061200990920     C     k01vtf01      klist
061300990920     C                   kfld                    vtftip
061400000626     C*
061500000626     C     k03vir02      klist
061600000626     C                   kfld                    virksc
061700000626     C                   kfld                    virtip
061800000626     C                   kfld                    virdti
061900000626     C*
062000000626     C     k02vir02      klist
062100000626     C                   kfld                    virksc
062200000626     C                   kfld                    virtip
062300000626     C*
062400000626     C     k02vss01      klist
062500000626     C                   kfld                    vsssun
062600020107     C                   kfld                    vssisv
062700080526     C*
062800080526     C     KEYvire1_C    klist
062900080526     C                   kfld                    virksc
063000080526     C                   kfld                    virtip
063100080526     C                   kfld                    virdti
063200080526     C                   kfld                    vireTRC
063300170731     C*
063400170731     C     KEYvav01_C    klist
063500170731     C                   kfld                    vavKEY1
063600170731     C                   kfld                    vavKEY2
063700170731     C                   kfld                    vavKEY3
063800990908     C*
063900990908     C                   endsr
