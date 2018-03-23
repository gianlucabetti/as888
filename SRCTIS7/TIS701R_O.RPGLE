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
001600990921     D prmtivlr        s                   like(tivlrds)
001700990910     D prmesito        s              1
001800000724     D prmlit          s             10    inz(*blanks)
001900000710     D prmfir          s             10    inz(*blanks)
002000021118     D prmfid          s             10    inz(*blanks)
002100990910     D wrkesito        s              1
002200000626     D dataiso         s               d
002300000626     D dataiso1        s               d
002400000626     D data8           s              8  0
002500000626     D ora             s              6  0
002600000626     D flgvirfound     s              1    inz('0')
002700000703     D wrkprg          s              8  0 inz(*zeros)
002800000703     D dwlprg          s             10    inz(*all'0')
002900030613     D savMSG          s             30    inz(*blanks)
003000020107
003100020107     D*------------------
003200020107     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300020107     D*------------------
003400020107     D wlbda8          DS                  INZ
003500020107     D  g08dat                 1      8  0
003600020107     D  g08inv                 9     16  0
003700020107     D  g08err                17     17
003800020107     D  g08tgi                18     22  0
003900020107     D*
004000000626
004100000626
004200990908     C* Leggo il record di log.
004300990908     C                   dou       %status = 0
004400990908     C     tivlrnrrp     chain(e)  tivlr00f
004500990908     C                   enddo
004600000630     C* Iniz. le variabili di wrk per reperimento informazioni abilitazione e regole cliente/file
004700000630     C                   exsr      inzvar
004800000711     C                   exsr      readtivir
004900021115     C* I seguenti campi vanno valorizzati solo in caso di NON data-entry guidato via Internet
005000021115     C                   if        vlrtia <> 'I'                                *se non data-entry I
005100021115     C                   eval      vlrpoi = virpoi
005200021115     C                   eval      vlrfl1 = virfi2
005300021115     C                   endif
005400010710     C* Controllo origine record di log (se da STRATEGI o procedura FTP/EMAIL ...
005500010710     C* ... o ancora da data-entry giudato via Internet)
005600000703     C                   if        vlrlis = *blanks or                          *trattasi op. di FTP
005700000703     C                             vlrfis = *blanks or
005800011127     C                             vlrfou = *blanks or
005900011127     C                             vlrpgt = *blanks
006000000711     C* Completo i campi mancanti nel record di log
006100000711     C                   eval      vlrlis = '*LIBL'
006200000711     C                   eval      vlrfis = virfis
006300011127     C                   eval      vlrfou = virfit
006400000711     C                   eval      vlrpgt = virpgt
006500000711     C                   eval      vlrppt = virppt
006600000626     C                   else
006700000627     C* Valorizzo progressivo applicazione solo se trattasi di record effettivamente nuovo.
006800010524     C                   if        vlrprg = *blanks
006900000627     C                   exsr      calprog
007000000627     C                   endif
007100000626     C                   clear                   wrkesito
007200030613     C***                clear                   vlrmsg
007300021119     C*
007400021119     C* Prima della traduzione disalloco il record
007500021119     C                   unlock    tivlr00f
007600021119     C*
007700990910     C* Traduzione.
007800990910     C                   exsr      traduci
007900021119     C*
008000021119     C* Subito dopo la traduzione salvo il buffer e rileggo il record
008100021119     C                   eval      sv_tivlrds = tivlrds
008200021119     C                   dou       %status = 0
008300021119     C     tivlrnrrp     chain(e)  tivlr00f
008400021119     C                   enddo
008500021119     C*
008600021119     C* Dopo aver letto memorizzo l'unico campo che nella traduzione può essere cambiato
008700021119     C* (vlrfl3 = flag operativi) e re-imposto il buffer salvato prima della rilettura del record.
008800021119     C                   eval      sv_vlrfl3 = vlrfl3
008900021119     C                   eval      tivlrds = sv_tivlrds
009000021119     C*
009100000626     C* Imposto stato record.
009200000626     C                   select
009300000626     C                   when      wrkesito = ' '
009400000626     C                   eval      vlrflg = '0'
009500000626     C                   when      wrkesito = '0'
009600000626     C                   eval      vlrflg = '1'
009700000626     C                   other
009800000626     C                   eval      vlrflg = '2'
009900000626     C                   endsl
010000010615     C*
010100000626     C                   endif
010200021115     C                   exsr      updtivlr
010300000626     C*
010400000627     C                   seton                                        lr
010500000626
010600000627
010700000626
010800000627      /TITLE Valorizzazione Progressivo Applicazione
010900000627     C     calprog       begsr
011000000627     C*
011100000711     C                   open(e)   tis7prgf
011200000711     C                   if        not %error
011300000627     C                   read      tis7prgf
011400000627     C*
011500000627     C                   eval      dwlprg = f_tis7prgf
011600000627     C                   move(p)   dwlprg        wrkprg
011700000627     C                   add       1             wrkprg
011800000627     C                   move(p)   wrkprg        dwlprg
011900000627     C                   movel     vlrisv        dwlprg
012000000627     C*
012100000627     C                   eval      f_tis7prgf = dwlprg
012200010524     C                   update(e) tis7prg0
012300010524     C                   if        not %error
012400010524     C                   eval      vlrprg = dwlprg
012500000711     C                   endif
012600010524     C                   endif
012700000711     C                   close(e)  tis7prgf
012800000627     C*
012900000627     C                   endsr
013000000627
013100000627
013200000626
013300000626      /TITLE Aggiornamento TIVLR00F.
013400000626     C     updtivlr      begsr
013500000630     C*
013600000626     C* Aggiorno record di log con l'esito dell'elaborazione.
013700000627     C*                  if        vlrflg <> '0'
013800000630     C*
013900000630     C* Mi riposiziono prima di effettuare un successivo aggiornamento.
014000990910     C                   update    tivlr000
014100000627     C*                  else
014200000627     C*                  unlock    tivlr00f
014300000627     C*                  endif
014400000626     C*
014500000626     C                   endsr
014600000626
014700000626
014800000623
014900000623      /TITLE Lettura TIVIR00F.
015000000626     C     readtivir     begsr
015100000623     C*
015200000626     C                   clear                   wrkesito
015300000626     C*
015400000626     C                   eval      vsssun = vlrsun
015500020107     C                   eval      vssisv = vlrisv
015600000623     C     k02vss01      chain     tivss01l
015700000623     C*
015800020107     C                   if        not %found or
015900020107     C                             vssdde > datcor or
016000020107     C                             vssdsc < datcor
016100000630     C                   eval      wrkesito='1'
016200030613     C*
016300030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
016400030613     C* proveniente da Procedura FTP/e-mail
016500030613     C                   if        vlrmsg <> *blanks
016600030613     C                   eval      savMSG = %trim(vlrmsg)
016700030613     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
016800030613     C                                      'al servizio.'
016900030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
017000030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
017100030613     C                   else
017200000630     C                   eval      vlrmsg = 'Errore, cliente non abilitato ' +
017300000630     C                                      'al servizio.'
017400030613     C                   endif
017500010615     C                   exsr      sndeml
017600000626     C                   else
017700010710     C*
017800010710     C* Costruisco la parte fissa della chiave
017900010710     C                   eval      virksc = vssksu
018000010710     C                   eval      virtip = vlrtip
018100010710     C                   move      dataiso       dataiso1
018200010710     C                   adddur    1:*d          dataiso1
018300010710     C     *iso          move      dataiso1      virdti
018400010626     C* Se trattasi di dati provenienti da inserimenti guidati via Internet =>
018500010710     C* ... non cerco una regola valida per il cliente/tipo file ma assumo che sia ok.
018600010710     C                   if        vlrtia = 'I'
018700030613     C***                clear                   tivir000
018800010710     C                   eval      flgvirfound = *on
018900010710     C                   else
019000010710     C*
019100000623     C                   reset                   flgvirfound
019200000623     C     k03vir02      setll     tivir02l
019300000623     C     k02vir02      readpe    tivir02l
019400000623     C* Se ho letto il record
019500000623     C* ed è valido segnalo che l'ho trovato.
019600000623     C                   if        not %eof
019700000623     C                             and data8 <= virdtf
019800000623     C                   eval      flgvirfound = *on
019900000630     C                   else
020000000630     C                   if        flgvirfound = *off
020100000630     C                   eval      wrkesito='1'
020200030613     C*
020300030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
020400030613     C* proveniente da Procedura FTP/e-mail
020500030613     C                   if        vlrmsg <> *blanks
020600030613     C                   eval      savMSG = %trim(vlrmsg)
020700030613     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
020800030613     C                                      ' per il cliente.'
020900030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
021000030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
021100030613     C                   else
021200030613     C                   eval      vlrmsg = 'Errore, tipo file non previsto' +
021300030613     C                                      ' per il cliente.'
021400030613     C                   endif
021500010615     C                   exsr      sndeml
021600000623     C                   endif
021700000630     C                   endif
021800010626     C*
021900010626     C                   endif
022000010626     C* In ogni caso =>
022100010626     C* ... reperisco regole standard.
022200000623     C                   if        flgvirfound = '1'
022300000623     C*
022400010726     C                   eval      vtftip = virtip
022500000623     C     k01vtf01      chain     tivtf01l
022600000623     C*
022700000623     C                   if        %found
022800000623     C*
022900000623     C                   if        virctl = *blanks
023000000623     C                   eval      virctl = vtfctl
023100000623     C                   endif
023200000623     C*
023300000623     C                   if        virpgt = *blanks
023400000623     C                   eval      virpgt = vtfpgt
023500000623     C                   endif
023600000725     C*
023700000725     C                   if        virppt = *blanks
023800000725     C                   eval      virppt = vtfppt
023900000725     C                   endif
024000000713     C*
024100000713     C                   if        virpgi = *blanks
024200000713     C                   eval      virpgi = vtfpgi
024300000713     C                   endif
024400000725     C*
024500000725     C                   if        virppi = *blanks
024600000725     C                   eval      virppi = vtfppi
024700000725     C                   endif
024800000623     C*
024900000623     C                   if        virfid = *blanks
025000000623     C                   eval      virfid = vtffid
025100000623     C                   endif
025200000623     C*
025300000623     C                   if        virfis = *blanks
025400000623     C                   eval      virfis = vtffis
025500000623     C                   endif
025600000623     C*
025700000623     C                   if        virfit = *blanks
025800000623     C                   eval      virfit = vtffit
025900000623     C                   endif
026000000623     C*
026100000630     C                   else
026200000630     C                   eval      wrkesito='1'
026300030613     C*
026400030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
026500030613     C* proveniente da Procedura FTP/e-mail
026600030613     C                   if        vlrmsg <> *blanks
026700030613     C                   eval      savMSG = %trim(vlrmsg)
026800030613     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
026900030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
027000030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
027100030613     C                   else
027200030613     C                   eval      vlrmsg = 'Errore, tipo file inesistente'
027300030613     C                   endif
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
029700990910     C*
029800990910     C                   select
029900000725     C* Se la call è finita in errore
030000000725     C* o la traduzione ha avuto degli errori (ma invio effettuato).
030100000725     C                   when      %error
030200000725     C                   eval      wrkesito = '1'
030300030613     C*
030400030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
030500030613     C* proveniente da Procedura FTP/e-mail
030600030613     C                   if        vlrmsg <> *blanks
030700030613     C                   eval      savMSG = %trim(vlrmsg)
030800030613     C                   eval      vlrmsg = 'Errori non identificati' +
030900030613     C                                      ' in fase di trad.'
031000030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
031100030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
031200030613     C                   else
031300030613     C                   eval      vlrmsg = 'Errori non identificati' +
031400030613     C                                      ' in fase di trad.'
031500030613     C                   endif
031600010615     C                   exsr      sndeml
031700000725     C* Se la traduzione ha avuto degli errori (ma invio effettuato).
031800000725     C                   when      prmesito = '1'
031900990910     C                   eval      wrkesito = '1'
032000030613     C*
032100030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
032200030613     C* proveniente da Procedura FTP/e-mail
032300030613     C                   if        vlrmsg <> *blanks
032400030613     C                   eval      savMSG = %trim(vlrmsg)
032500030613     C                   eval      vlrmsg = 'Invio effettuato,' +
032600030613     C                                      ' ma errori nella trad.'
032700030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
032800030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
032900030613     C                   else
033000030613     C                   eval      vlrmsg = 'Invio effettuato,' +
033100030613     C                                      ' ma errori nella trad.'
033200030613     C                   endif
033300000725     C* Se la traduzione ha avuto degli errori (ed invio non effettuato).
033400000725     C                   when      prmesito = '2'
033500000710     C                   eval      wrkesito = '1'
033600030613     C*
033700030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
033800030613     C* proveniente da Procedura FTP/e-mail
033900030613     C                   if        vlrmsg <> *blanks
034000030613     C                   eval      savMSG = %trim(vlrmsg)
034100030613     C                   eval      vlrmsg = 'Errori in trad. ed' +
034200030613     C                                      ' invio non effettuato'
034300030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
034400030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
034500030613     C                   else
034600030613     C                   eval      vlrmsg = 'Errori in trad. ed' +
034700030613     C                                      ' invio non effettuato'
034800030613     C                   endif
034900010615     C                   exsr      sndeml
035000000725     C* Se la sottomissione dell'invio al PO non è riuscita.
035100000725     C                   when      prmesito = '3'
035200990921     C                   eval      wrkesito = '1'
035300030613     C*
035400030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
035500030613     C* proveniente da Procedura FTP/e-mail
035600030613     C                   if        vlrmsg <> *blanks
035700030613     C                   eval      savMSG = %trim(vlrmsg)
035800030613     C                   eval      vlrmsg = 'Errori sottomissione invio a PO.'
035900030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
036000030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
036100030613     C                   else
036200030613     C                   eval      vlrmsg = 'Errori sottomissione invio a PO.'
036300030613     C                   endif
036400010615     C                   exsr      sndeml
036500010615     C* Se la il pgm traduttore non esiste.
036600010615     C                   when      prmesito = '4'
036700010615     C                   eval      wrkesito = '1'
036800030613     C*
036900030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
037000030613     C* proveniente da Procedura FTP/e-mail
037100030613     C                   if        vlrmsg <> *blanks
037200030613     C                   eval      savMSG = %trim(vlrmsg)
037300030613     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
037400030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
037500030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
037600030613     C                   else
037700030613     C                   eval      vlrmsg = 'Errore, traduttore inesistente.'
037800030613     C                   endif
037900010615     C                   exsr      sndeml
038000010615     C* Se la chiamata al pgm traduttore fallisce.
038100010615     C                   when      prmesito = '5'
038200010615     C                   eval      wrkesito = '1'
038300030613     C*
038400030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
038500030613     C* proveniente da Procedura FTP/e-mail
038600030613     C                   if        vlrmsg <> *blanks
038700030613     C                   eval      savMSG = %trim(vlrmsg)
038800030613     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
038900030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
039000030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
039100030613     C                   else
039200030613     C                   eval      vlrmsg = 'Errori, in chiamata traduttore.'
039300030613     C                   endif
039400010615     C                   exsr      sndeml
039500010615     C* Se la traduzione "multi-filiale" ha avuto degli errori (occorre verificare).
039600010615     C                   when      prmesito = '6'
039700010615     C                   eval      wrkesito = '1'
039800030613     C*
039900030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
040000030613     C* proveniente da Procedura FTP/e-mail
040100030613     C                   if        vlrmsg <> *blanks
040200030613     C                   eval      savMSG = %trim(vlrmsg)
040300030613     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
040400030613     C                                      'invio effettuato ma verif.'
040500030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
040600030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
040700030613     C                   else
040800030613     C                   eval      vlrmsg = 'Errori in invio multiplo, ' +
040900030613     C                                      'invio effettuato ma verif.'
041000030613     C                   endif
041100010615     C                   exsr      sndeml
041200021118     C* Se la storicizzazione ha avuto degli errori (occorre verificare).
041300021118     C                   when      prmesito = '7'
041400021118     C                   eval      wrkesito = '1'
041500030613     C*
041600030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
041700030613     C* proveniente da Procedura FTP/e-mail
041800030613     C                   if        vlrmsg <> *blanks
041900030613     C                   eval      savMSG = %trim(vlrmsg)
042000030613     C                   eval      vlrmsg = 'Errori in storicizzazione '
042100030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
042200030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
042300030613     C                   else
042400030613     C                   eval      vlrmsg = 'Errori in storicizzazione '
042500030613     C                   endif
042600021118     C                   exsr      sndeml
042700000725     C* Se la traduzione è stata eseguita senza errori.
042800990910     C                   when      not %error and prmesito = '0'
042900990910     C                   eval      wrkesito = '0'
043000030613     C*
043100030613     C* Gestisco l'eventuale contenuto del campo messaggio => se pieno contiene il nome del file
043200030613     C* proveniente da Procedura FTP/e-mail
043300030613     C                   if        vlrmsg <> *blanks
043400030613     C                   eval      savMSG = %trim(vlrmsg)
043500030613     C                   eval      vlrmsg = 'Upload eseguito con successo.'
043600030613     C                   eval      %subst(vlrmsg:80-%len(%trim(savMSG)):
043700030613     C                             %len(%trim(savMSG))) = %trim(savMSG)
043800030613     C                   else
043900030613     C                   eval      vlrmsg = 'Upload eseguito con successo.'
044000030613     C                   endif
044100990910     C                   endsl
044200990910     C*
044300990910     C                   endsr
044400990910
044500000626
044600000626
044700000626      /TITLE Inizializzazioni variabili.
044800000626     C     inzvar        begsr
044900000626     C*
045000000626     C                   time                    dataiso
045100000626     C     *iso          move      dataiso       data8
045200000626     C                   time                    ora
045300000626     C*
045400000626     C                   endsr
045500000626
045600010615
045700010615
045800010615
045900010615      /TITLE Compongo il testo e spedisco una e-m@ail
046000010615     C     sndeml        begsr
046100010615     C*
046200010615     C* Inizializzo variabili
046300010911     C                   movel     *blanks       wrkEml          253
046400010911     C                   movel     *blanks       wrkMsg         5000
046500010911     C                   movel     *blanks       wrkOgg           44
046600010615     C* Valorizzo i campi della e-m@ail
046700120305     C                   eval      wrkEml='ced@brt.it'
046800010911     C                   eval      wrkOgg='VAS - WARNING!!!'
046900010615     C                   eval      wrkMsg='VERIFICARE IL LOG UPLOAD '  +
047000010615     C                             'SU AS/400'                         +':/N'+
047100010615     C                             ' '                                 +':/N'+
047200010615     C                             'RIFERIMENTI'                       +':/N'+
047300010615     C                             'Data:'+%editw(vlrdat:'    /  /  ') +':/N'+
047400010615     C                             'Ora:'+%editw(vlrora:'  :  :  ')    +':/N'+
047500010615     C                             'Cliente:'+vlrksc                   +':/N'+
047600010615     C                             'TipoFile:'+vlrtip                  +':/N'+
047700010615     C                             'Traduttore:'+vlrpgt                +':/N'+
047800010615     C                             'Parametri:'+vlrppt                 +':/N'+
047900010615     C                             ' '                                 +':/N'+
048000010615     C                             'MESSAGGIO'                         +':/N'+
048100010615     C                             vlrmsg
048200010615     C*
048300010615     C                   call(e)   'TIS701C'
048400010615     C                   parm                    wrkEml
048500010911     C                   parm                    wrkOgg
048600010615     C                   parm                    wrkMsg
048700020118     C*
048800020118     C* === AGGIUNTA X AVVISARE LED X I CLIENTI LOGISTICA ===
048900020118     C                   if        %subst(vlrksc:2:3) = '199' or
049000020118     C                             %subst(vlrksc:2:3) = '200' or
049100020118     C                             %subst(vlrksc:2:3) = '207' or
049200020118     C                             %subst(vlrksc:2:3) = '208'
049300020118     C*
049400120305     C                   eval      wrkEml='info199@brt.it'
049500020118     C*
049600020118     C                   call(e)   'TIS701C'
049700020118     C                   parm                    wrkEml
049800020118     C                   parm                    wrkOgg
049900020118     C                   parm                    wrkMsg
050000020118     C*
050100020118     C                   endif
050200020118     C* =====================================================
050300010615     C*
050400010615     C                   endsr
050500010615
050600010615
050700000626
050800000626
050900990908      /TITLE Operazioni iniziali.
051000990908     C     *inzsr        begsr
051100990908     C*
051200990908     C     *entry        plist
051300990908     C                   parm                    tivlrnrrp
051400000710     C                   parm                    virpgi
051500000710     C                   parm                    virppi
051600000717     C                   parm                    virfit
051700000717     C                   parm                    virfi3
051800000717     C                   parm                    virfi4
051900020107     C*
052000020107     C* Reperimento data corrente
052100020107     C                   time                    wn14             14 0
052200020107     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052300020107     C                   z-add     wn8           g08dat
052400020107     C                   z-add     *zeros        g08inv
052500020107     C                   movel     '0'           g08err
052600020107     C                   call      'XSRDA8'
052700020107     C                   parm                    wlbda8
052800020107     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052900990920     C*
053000990920     C     k01vtf01      klist
053100990920     C                   kfld                    vtftip
053200000626     C*
053300000626     C     k03vir02      klist
053400000626     C                   kfld                    virksc
053500000626     C                   kfld                    virtip
053600000626     C                   kfld                    virdti
053700000626     C*
053800000626     C     k02vir02      klist
053900000626     C                   kfld                    virksc
054000000626     C                   kfld                    virtip
054100000626     C*
054200000626     C     k02vss01      klist
054300000626     C                   kfld                    vsssun
054400020107     C                   kfld                    vssisv
054500990908     C*
054600990908     C                   endsr
