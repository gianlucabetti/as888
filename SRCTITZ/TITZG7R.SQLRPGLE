000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400130116      * PARTICOLARITA':
000500130116
000600020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000700100412     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900150917     Ftabel00f  if   e           k disk
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800150917     D ds15          e ds
001900070730     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500150917     D jNAZ            s              5  0 INZ(*zeros)
002600011113     D depcmd          s            150    INZ(*blanks)
002700150917     D depspe          s             14    INZ(*blanks)
002800150917     D curspe          s             14    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400091223     D Num5_0          s              5  0
003500020725
003600020725     D*------------------
003700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003800020725     D*------------------
003900070530     D INPUT_DS        DS                  INZ
004000100729     D  VINDTA                     2048
004100100729     D  VINFLG                        1
004200150917     D  VINSPE                       14
004300100729     D  VINRRN                        8  0
004400020725     D*
004500081113
004600091223     D*------------------
004700091223     D* DS REPERIMENTO NUMERATORE
004800091223     D*------------------
004900100309     D trul33ds      e ds                  inz
005000091223     D*------------------
005100091223     D* DS ARCHITETTURA
005200091223     D*------------------
005300091223     D kpjba         e ds                  inz
005400091223
005500150917     D*------------
005600150917     D* SCHIERE
005700150917     D*------------
005800150917     D skNAZISO        S              3    DIM(1000)
005900150917     D skNAZBAR        S              3    DIM(1000)
006000101119
006100101119     D*-------------------
006200101119     D* COSTANTI
006300101119     D*-------------------
006400101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006500101119     D                                     èéù')
006600101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006700101119     D                                     EEU')
006800081113
006900081113     D*------------------
007000081113     D* LINKING A DEFINIZIONI ESTERNE
007100081113     D*------------------
007200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007300090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400081113
007500990908
007600010201
007700010201
007800080117     C*
007900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000080117     C
008100080117     C/EXEC SQL
008200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008300080117     C/END-EXEC
008400080117     C
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800000613     C*
008900150917     C                   EXSR      CARTAB                                       CARICA TABELLE
009000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
009100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009200000613     C*
009300010202     C* Effettuo la chiamata al CLLE preposto
009400100525     C  N51              call(e)   'TITVVTC'
009500100525     C                   parm                    parccm
009600100525     C                   parm                    parmbr
009700100525     C                   parm      '2'           paropz
009800100525     C   51              call(e)   'TITVVBC'
009900100525     C                   parm                    parccm
010000100525     C                   parm                    parmbr
010100100525     C                   parm      '2'           paropz
010200070730     C*
010300070730     C* Effettuo lancio TISI95 solo x chiusura
010400070730     C                   CLEAR                   TISI95DS
010500070730     C                   EVAL      I95TLA = 'C'
010600070730     C                   CALL      'TISI95R'
010700070730     C                   PARM                    TISI95DS
010800000616     C*
010900000801     C
011000010201     C                   seton                                        LR
011100990908
011200000801
011300910830     C*--------------------------------------------------------
011400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
011500910830     C*--------------------------------------------------------
011600070530     C     RWFILE        BEGSR
011700990910     C*
011800990914     C                   if        not %open(tivin00r)
011900990908     C                   open      tivin00r
012000990914     C                   endif
012100070530     C*
012200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
012300100525     C                   exsr      prevasx
012400010201     C*
012500010202     C                   if        chkcall = '0'
012600010202     C*
012700100525     C                   if        not %open(fivabwwr)
012800100525     C                   open      fivabwwr
012900100525     C                   endif
013000100525     C*
013100021113     C                   if        not %open(fivatwwr)
013200021113     C                   open      fivatwwr
013300010201     C                   endif
013400080117     C*
013500080117     C                   EXSR      INZVAR
013600080117     C                   EXSR      DEFCAM
013700990910     C*
013800010201     C                   clear                   §CTROKVB          5 0
013900020305     C                   clear                   §CTROKVT          5 0
014000000801     C                   clear                   §CTRMO            5 0
014100000801     C                   clear                   §CTRNO            5 0
014200990910     C*
014300020725     C/EXEC SQL
014400020725     C+ declare C1 cursor for select
014500150917     C+ vindta, vinflg, substr(vindta, 46, 14) as sped, rrn(tivin00r)
014600060223     C+ from tivin00r
014700111020     C+ order by sped
014800020725     C+ for read only
014900020725     C/END-EXEC
015000020725     C
015100020725     C/EXEC SQL
015200020725     C+ open C1
015300020725     C/END-EXEC
015400020725     C
015500020725     C/EXEC SQL
015600070530     C+ Fetch C1 into :INPUT_DS
015700020725     C/END-EXEC
015800020725     C*
015900020725     C                   dow       sqlcod = *zeros
016000990913     C*
016100100310     C                   if        vindta > *blanks
016200000613     C                   add       1             rrnum
016300000801     C*
016400020725     C                   if        vinflg = *blanks
016500020725     C                             or vinflg = '0'
016600020725     C                             or vinflg = '2'
016700000801     C*
016800020725     C                   clear                   x_vinmsg
016900020725     C                   eval      x_vinflg = '1'
017000101119     C*
017100101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
017200101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017300010305     C*
017400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017500150917     C                   EVAL      curspe=%trim(%subst(vindta:46:14))
017600010305     C*
017700100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017800080923     C                   if        *in50 = *off
017900101119     C                   exsr      inzvar
018000101119     C                   exsr      defcam
018100071003     C                   exsr      impvab
018200100525     C                   exsr      wrivab
018300111031     C                   exsr      exevata                                      => carico VAT
018400111031     C                   exsr      exevatb                                      => carico VAT
018500111031     C   51              exsr      exevate                                      => carico VAT
018600150917     C                   exsr      exevati                                      => carico VAT
018700071003     C                   else
018800080923     C*
018900071009     C                   if        wDISK = 'D'
019000091223     C                   exsr      repNSP
019100071009     C                   exsr      impvab
019200100412     C                   exsr      wrivab
019300111031     C                   exsr      exevata                                      => carico VAT
019400111031     C                   exsr      exevatb                                      => carico VAT
019500111031     C                   exsr      exevate                                      => carico VAT
019600150917     C                   exsr      exevati                                      => carico VAT
019700071009     C                   else
019800071009     C*
019900010305     C                   if        depspe = *blanks                             => 1° giro
020000010305     C                   eval      depspe = curspe                              => memorizz. spediz
020100080117     C                   clear                   tivinds
020200091223     C                   exsr      repNSP
020300020305     C                   exsr      impvab
020400111031     C                   exsr      exevata                                      => carico VAT
020500111031     C                   exsr      exevatb                                      => carico VAT
020600111031     C   50              exsr      exevate                                      => carico VAT
020700150917     C                   exsr      exevati                                      => carico VAT
020800010305     C                   else
020900020725     C                   if        depspe <> curspe                             => rottura di spediz
021000010305     C                   eval      depspe = curspe                              => memorizz. spediz
021100100412     C                   exsr      wrivab
021200080117     C                   clear                   tivinds
021300091223     C                   exsr      repNSP
021400020305     C                   exsr      impvab
021500111031     C                   exsr      exevata                                      => carico VAT
021600111031     C                   exsr      exevatb                                      => carico VAT
021700111031     C   50              exsr      exevate                                      => carico VAT
021800150917     C                   exsr      exevati                                      => carico VAT
021900020305     C                   else                                                   => x stessa spediz
022000100401     C*
022100100401     C* Se nn richiesta esclusione spedizioni "duplicate"
022200100928     C                   select
022300100928     C                   when      wDUPREC = ' '
022400020305     C                   exsr      impvab
022500111031     C                   exsr      exevata                                      => carico VAT
022600111031     C                   exsr      exevatb                                      => carico VAT
022700111031     C   50              exsr      exevate                                      => carico VAT
022800150917     C                   exsr      exevati                                      => carico VAT
022900100928     C                   when      wDUPREC = 'C'
023000111031     C   50              exsr      exevate                                      => carico VAT
023100100928     C                   endsl
023200010305     C                   endif
023300010305     C                   endif
023400010305     C                   endif
023500071003     C                   endif
023600110201     C*
023700071009     C                   endif
023800000905     C*
023900000905     C                   else
024000020725     C                   eval      x_vinflg = '1'
024100000905     C                   endif
024200000905     C*
024300020725     C     VINRRN        chain     tivin000
024400020725     C                   if        %found(tivin00r)
024500020725     C                   eval      y_vinflg = x_vinflg
024600020725     C                   eval      y_vinmsg = x_vinmsg
024700020725     C                   update    tivin000
024800020725     C                   endif
024900020725     C*
025000020725     C/EXEC SQL
025100070530     C+ Fetch C1 into :INPUT_DS
025200020725     C/END-EXEC
025300020725     C*
025400020725     C                   enddo
025500020725     C*
025600020725     C/EXEC SQL
025700020725     C+ close C1
025800020725     C/END-EXEC
025900000905     C*
026000020305     C* Scarico i VAB rimasti "in sospeso"
026100071009     C                   if        wDISK = 'C'
026200100412     C                   exsr      wrivab
026300071009     C                   endif
026400010202     C*
026500010202     C                   endif
026600990910
026700990910     C* Se non ci sono record con errori ...
026800000710     C                   if        §ctrno = 0
026900990910     C* ... restituisco esito OK.
027000990921     C                   eval      wrkesito = '0'
027100990910     C                   else
027200100525     C                   if        §ctrokvb > 0 OR
027300100525     C                             §ctrokvt > 0
027400990921     C                   eval      wrkesito = '1'
027500000710     C                   else
027600000710     C                   eval      wrkesito = '2'
027700990910     C                   endif
027800000710     C                   endif
027900990910     C*
028000990914     C                   if        %open(tivin00r)
028100990908     C                   close     tivin00r
028200990914     C                   endif
028300021113     C                   if        %open(fivabwwr)
028400021113     C                   close     fivabwwr
028500990914     C                   endif
028600021113     C                   if        %open(fivatwwr)
028700021113     C                   close     fivatwwr
028800010201     C                   endif
028900990910     C*
029000100525     C                   if        §ctrokvb > 0 OR
029100100525     C                             §ctrokvt > 0
029200000724     C                             and vlrpoi <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800100412
029900100412
030000010305
030100010305     C*----------------------------------------------------*
030200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030300010305     C*----------------------------------------------------*
030400020305     C     WRIVAB        BEGSR
030500100412     C*
030600100412     C* Gestisco l'eventuale rottura x numero spedizione
030700100412     C                   if        VABRMA = *blanks
030800100412     C                   movel(P)  VABRMN        VABRMA
030900100412     C                   endif
031000100412     C*
031100100525     C                   if        *in51 = *off and *in31 = *off
031200100525     C                   write     fivab000                                     => scarico il VAB
031300100525     C                   endif
031400010305     C*
031500010305     C                   ENDSR
031600990920
031700000801     C*----------------------------------------------------*
031800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900000801     C*----------------------------------------------------*
032000010201     C     INZVAR        BEGSR
032100000801     C*
032200010201     C                   Z-ADD     *zeros        Num5_0
032300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032400020725     C                   MOVEL     '0'           FlgCAS            1
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032900000801     C*----------------------------------------------------*
033000020904     C     DEFCAM        BEGSR
033100080923     C*
033200100928     C                   SETOFF                                       505160
033300000801     C*
033400021113     C                   CLEAR                   FIVAB000
033500021113     C                   CLEAR                   FIVAT000
033600070730     C* Imposto i valori di default...
033700150917     C                   EVAL      VABCCM = 0058090
033800150917     C                   EVAL      VATCCM = 0058090
033900140325     C                   EVAL      VABLNP = 005
034000140325     C                   EVAL      VATLNP = 005
034100150917     C                   EVAL      VABCTR = 399
034200140325     C                   EVAL      VABCTM = '7Q'
034300140325     C                   EVAL      VABCBO = '1'
034400070222     C* ... e poi verifico se sono stati passati come parametri
034500070222     C                   IF        vlrppt > *blanks
034600100525     C*
034700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABCCM
035200070222     C                   Z-ADD     PiVal         VATCCM
035300070222     C                   ENDIF
035400070222     C                   ENDIF
035500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABLNP
036000070222     C                   Z-ADD     PiVal         VATLNP
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABCTR
036800070222     C                   ENDIF
036900070928     C                   ENDIF
037000071009     C                   MOVEL     *blanks       wDISK             1
037100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037200071009     C                   IF        wDISK <> *blanks
037300070928     C                   SETON                                        50
037400070222     C                   ENDIF
037500100525     C                   IF        wDISK  = 'T'
037600100525     C                   SETOFF                                       50
037700100525     C                   SETON                                        51
037800100525     C                   ENDIF
037900100401     C                   MOVEL     *blanks       wDUPREC           1
038000100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
038100100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
038200100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038300100525     C                   ENDIF
038400111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
038500111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
038600100525     C*
038700070222     C                   ENDIF
038800071009     C*
038900071009     C   50              EVAL      VABCTM = '7Q'
039000000801     C*
039100000801     C                   ENDSR
039200150917
039300150917     C*--------------------------------------------------------
039400150917     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
039500150917     C*--------------------------------------------------------
039600150917     C     CARTAB        BEGSR
039700150917     C*
039800150917     C* TABELLA '15' - NAZIONI
039900150917     C                   clear                   skNAZISO
040000150917     C                   clear                   skNAZBAR
040100150917     C                   eval      tblKUT = 1
040200150917     C                   eval      tblCOD = '15'
040300150917     C     KEYtabP       setll     tabel00f
040400150917     C     KEYtabP       reade     tabel00f
040500150917     C                   dow       not %eof(tabel00f)
040600150917     C                   if        tblFLG = *blanks
040700150917     C                   movel(p)  tblUNI        ds15
040800150917     C                   add       1             jNAZ
040900150917     C                   eval      skNAZBAR(jNAZ) = tblKEY
041000150917     C                   eval      skNAZISO(jNAZ) = §15COD
041100150917     C                   endif
041200150917     C     KEYtabP       reade     tabel00f
041300150917     C                   enddo
041400150917     C*
041500150917     C                   ENDSR
041600150917
041700091223     C*----------------------------------------------------*
041800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041900091223     C*----------------------------------------------------*
042000091223     C     REPNSP        BEGSR
042100091223     C*
042200091223     C                   EXSR      INZVAR
042300091223     C                   EXSR      DEFCAM
042400100928     C*
042500100928     C                   SETON                                        60
042600091223     C*
042700091223     C* NSP => Stacco un numeratore da AZNUM
042800091223     C                   clear                   TRUL33DS
042900091223     C                   eval      I33OPE = *zeros
043000091223     C                   eval      I33CNU = 302
043100091223     C                   eval      I33NUM = 1
043200091223     C                   movel     TRUL33DS      KPJBU
043300091223     C                   call      'TRUL33R'
043400091223     C                   parm                    KPJBA
043500091223     C                   movel     KPJBU         TRUL33DS
043600091223     C                   if        O33ERR = *zeros
043700091223     C                   z-add     O33NRF        VABNSP
043800091223     C                   z-add     O33NRF        VATNSP
043900091223     C                   else
044000091223     C                   SETON                                        31
044100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200091223     C                             + ' ' + 'VABNSP VATNSP'
044300091223     C                   endif
044400091223     C*
044500091223     C                   ENDSR
044600111031     C*----------------------------------------------------*
044700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044800111031     C*----------------------------------------------------*
044900111031     C     IMPVAB        BEGSR
045000111031     C*
045100111031     C                   Z-ADD     *zeros        errore            1 0
045200111031     C                   MOVEL     datcor        VABAAS
045300111031     C                   MOVEL     datcor        VATAAS
045400111031     C                   MOVE      datcor        VABMGS
045500111031     C                   MOVE(P)   vlrpoi        VABFGS
045600111031     C                   MOVE(P)   vlrpoi        VATFGS
045700111031     C*
045800150917     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
045900111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046000111031     C     '@':'A'       XLATE     VABRSD        VABRSD
046100111031     C* ==
046200150917     C                   IF        %subst(vindta:276+35:5) <> *blank
046300150917     C                   EVAL      VABRD2=%trim(%subst(vindta:276+35:5))
046400150917     C                   ENDIF
046500150917     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:40)))
046600150917     C* QUESTO È LO STD SDA (lasciato per la gestione italiana, forse...)
046700150917     C* Trimmare i contenuti dei 3 singoli campi concatenandoli tra loro separati da 1 *blank. Se il
046800150917     C* valore concatenato risultante è più lungo di 35 bytes allora fare la stessa cosa ma solo
046900150917     C* sugli ultimi 2 campi. Quindi attribuire tale valore a VABIND
047000150917      ***
047100150917     C***                IF        %len(%trim(%trim(%subst(vindta:336:5))+' '+
047200150917     C***                               %trim(%subst(vindta:341:30))+' '+
047300150917     C***                               %trim(%subst(vindta:371:5)))) <= 35
047400150917     C***                EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
047500150917     C***                                 %trim(%subst(vindta:341:30))+' '+
047600150917     C***                                 %trim(%subst(vindta:371:5)))
047700150917     C***                ELSE
047800150917     C***                EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
047900150917     C***                                 %trim(%subst(vindta:341:30)))
048000150917     C***                ENDIF
048100150917     C
048200111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
048300111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
048400140325     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
048500111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
048600111031     C                   IF        %subst(vindta:43:1) = 'P'
048700111031     C                   EVAL      VABCBO='2'
048800111031     C                   ENDIF
048900140325     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
049000140325     C                   EVAL      VABNZD=%trim(%subst(vindta:615:3))
049100150917     C* per la nazione consideriamo il dato come la nazione ISO e poi la
049200150917     C* sostituiamo con la nostra
049300150917     C                   Z-ADD     1             jNAZ
049400150917     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
049500150917     C                   IF        %found
049600150917     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
049700150917     C                   ENDIF
049800150917     C* se la nazione è straniera
049900150917     C                   IF        VABNZD <> *blank
050000150917     C* il trattamento merce è 7R
050100150917     C                   EVAL      VABCTM = '7R'
050200150917     C                   ENDIF
050300150917     C                   EVAL      VABVCA = %trim(%subst(vindta:500:2))
050400150917     C* se VCA è EU da file diventa EUR altrimenti resta uguale
050500150917     C                   IF        VABVCA = 'EU'
050600150917     C                   EVAL      VABVCA = 'EUR'
050700150917     C                   ENDIF
050800150917     C                   SELECT
050900150917     C                   WHEN      %subst(vindta:511:3) = 'CON'
051000150917     C                   EVAL      VABTIC=*blank
051100150917     C                   WHEN      %subst(vindta:511:3) = 'ABM'
051200150917     C                   EVAL      VABTIC='BM'
051300150917     C                   WHEN      %subst(vindta:511:3) = 'ABS'
051400150917     C                   EVAL      VABTIC='BB'
051500150917     C                   WHEN      %subst(vindta:511:3) = 'ACM'
051600150917     C                   EVAL      VABTIC='CM'
051700150917     C                   ENDSL
051800150917     C* CAP (il cliente fa export per cui si prende il dato così com'è
051900150917     C                   EVAL      VABCAD=%trim(%subst(vindta:391:9))
052000150917     C***                EVAL      PiStr=%trim(%subst(vindta:391:9))
052100150917     C***                EXSR      CHKNUM
052200150917     C***                IF        PiInt=*on
052300150917     C***                Z-ADD     PiVal         Num5_0
052400150917     C***                MOVEL(P)  Num5_0        VABCAD
052500150917     C***                ELSE
052600150917     C***                ADD       1             errore
052700150917     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
052800150917     C***                          + ' ' + 'VABCAD'
052900150917     C***                ENDIF
053000111031     C* Reperisco la provincia dal CAP e dalla località
053100111031     C                   IF        VABCAD <> *blanks AND
053200111031     C                             VABPRD  = *blanks
053300111031     C                   CLEAR                   TISI95DS
053400111031     C                   EVAL      I95TCN = '3'
053500111031     C                   Z-ADD     datcor        I95DAT
053600111031     C                   EVAL      I95CAP = VABCAD
053700111031     C                   EVAL      I95LOC = VABLOD
053800111031     C                   EVAL      I95NAR = VABNZD
053900111031     C                   CALL      'TISI95R'
054000111031     C                   PARM                    TISI95DS
054100111031     C                   EVAL      VABPRD = O95PRV
054200111031     C                   ENDIF
054300111031     C* NCL
054400111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
054500111031     C                   EXSR      CHKNUM
054600111031     C                   IF        PiInt=*on
054700111031     C                   SELECT
054800111031     C                   WHEN      wNCL = *blanks
054900111031     C                   Z-ADD     PiVal         VABNCL
055000111031     C                   WHEN      wNCL = 'A'
055100111031     C                   ADD       PiVal         VABNCL
055200111031     C                   WHEN      wNCL = '1'
055300111031     C                   ADD       1             VABNCL
055400111031     C                   ENDSL
055500111031     C                   ELSE
055600111031     C                   ADD       1             errore
055700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800111031     C                             + ' ' + 'VABNCL'
055900111031     C                   ENDIF
056000111031     C* PKB
056100120403     C* il peso è in grammi
056200111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
056300111031     C                   EXSR      CHKNUM
056400111031     C                   IF        PiNum=*on
056500120403     C                   EVAL      PiVal=PiVal/1000
056600111031     C                   Z-ADD     PiVal         VABPKB
056700111031     C                   ELSE
056800111031     C                   ADD       1             errore
056900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057000111031     C                             + ' ' + 'VABPKB'
057100111031     C                   ENDIF
057200111031     C* RMN
057300120403     C*** elenco delle posizioni utilizzate nel tempo
057400150917     C                   EVAL      PiStr=%trim(%subst(vindta:46:15))
057500150917     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
057600121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
057700120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
057800120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
057900111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
058000111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
058100111031     C                   EXSR      CHKNUM
058200111031     C                   IF        PiInt=*on
058300111031     C                   Z-ADD     PiVal         VABRMN
058400111031     C                   ELSE
058500111031     C                   ADD       1             errore
058600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058700111031     C                             + ' ' + 'VABRMN'
058800111031     C                   ENDIF
058900111031     C* CAS
059000111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
059100130212     C                             %trim(%subst(vindta:502:9))<>'0000000.0' AND
059200111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
059300111031     C                             %trim(%subst(vindta:502:9))<>*zeros
059400111031     C                   EVAL      FlgCAS = '1'
059500111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
059600111031     C                   EXSR      CHKNUM
059700111031     C                   IF        PiNum=*on
059800111031     C                   Z-ADD     PiVal         VABCAS
059900111031     C                   ELSE
060000111031     C                   ADD       1             errore
060100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060200111031     C                             + ' ' + 'VABCAS'
060300111031     C                   ENDIF
060400111031     C                   ENDIF
060500111031     C* IAS
060600150917     C                   IF        %trim(%subst(vindta:543:10)) <> *blanks and
060700150917     C                             %trim(%subst(vindta:543:10)) <> *zeros
060800111031     C                   EVAL      VABVAS = 'EUR'
060900150917     C                   EVAL      PiStr=%trim(%subst(vindta:543:10))
061000111031     C                   EXSR      CHKNUM
061100111031     C                   IF        PiNum=*on
061200111031     C                   Z-ADD     PiVal         VABIAS
061300111031     C                   ELSE
061400111031     C                   ADD       1             errore
061500111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061600111031     C                             + ' ' + 'VABIAS'
061700111031     C                   ENDIF
061800111031     C                   ENDIF
061900111031     C*
062000111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
062100111031     C                   IF        FlgCAS <> '0'
062200111031     C                   IF        VABCBO = '1'
062300111031     C                   EVAL      VABCBO = '4'
062400111031     C                   ELSE
062500111031     C                   EVAL      VABCBO = '6'
062600111031     C                   ENDIF
062700111031     C                   ENDIF
062800111031     C*
062900111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
063000111031     C                   EXSR      CHKIMPDIV
063100111031     C*
063200111031     C* Ebbene...
063300111031     C                   ADD       1             §CTRMO
063400111031     C                   IF        errore <> *zeros
063500111031     C                   ADD       1             §CTRNO
063600111031     C                   EVAL      vinflg = '2'
063700111031     C                   ELSE
063800111031     C                   ADD       1             §CTROKVB
063900111031     C                   ENDIF
064000111031     C*
064100111031     C                   ENDSR
064200111031
064300111031
064400111031     C*----------------------------------------------------*
064500111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064600111031     C*----------------------------------------------------*
064700111031     C     CHKIMPDIV     BEGSR
064800111031     C*
064900111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065000111031     C                   Z-ADD     *zeros        wrkDec            9 9
065100111031     C*
065200111031     C* Come prima cosa effettuo considerazioni sulla divisa
065300111031     C                   IF        vabIAS > *zeros
065400111031     C                   IF        vabVAS <> 'EUR'
065500111031     C                   EVAL      vabVAS =  'ITL'
065600111031     C                   ENDIF
065700111031     C                   ENDIF
065800111031     C*
065900111031     C                   IF        vabCAS > *zeros
066000111031     C                   IF        vabVCA <> 'EUR'
066100111031     C                   EVAL      vabVCA =  'ITL'
066200111031     C                   ENDIF
066300111031     C                   ENDIF
066400111031     C*
066500111031     C                   IF        vabVMD > *zeros
066600111031     C                   IF        vabVAD <> 'EUR'
066700111031     C                   EVAL      vabVAD =  'ITL'
066800111031     C                   ENDIF
066900111031     C                   ENDIF
067000111031     C*
067100111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067200111031     C                   Z-ADD     vabIAS        wrkDec
067300111031     C                   IF        wrkDec > *zeros
067400111031     C                   IF        vabVAS = 'ITL'
067500111031     C                   EVAL      vabIAS = *zeros
067600111031     C                   ENDIF
067700111031     C                   ENDIF
067800111031     C*
067900111031     C* Stabilisco se il contrasegno ha decimali valorizzati
068000111031     C                   Z-ADD     vabCAS        wrkDec
068100111031     C                   IF        wrkDec > *zeros
068200111031     C                   IF        vabVCA = 'ITL'
068300111031     C                   EVAL      vabCAS = *zeros
068400111031     C                   ENDIF
068500111031     C                   ENDIF
068600111031     C*
068700111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068800111031     C                   Z-ADD     vabVMD        wrkDec
068900111031     C                   IF        wrkDec > *zeros
069000111031     C                   IF        vabVAD = 'ITL'
069100111031     C                   EVAL      vabVMD = *zeros
069200111031     C                   ENDIF
069300111031     C                   ENDIF
069400111031     C*
069500111031     C                   ENDSR
069600111031     C***
069700111031
069800111031
069900111031     C*----------------------------------------------------*
070000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
070100111031     C*----------------------------------------------------*
070200111031     C     EXEVATA       BEGSR
070300111031     C*
070400140325     C                   EVAL      VATTRC='A'
070500140325     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
070600140325     C                   exsr      wrivat                                       => scarico VAT
070700111031     C*
070800111031     C                   ENDSR
070900111031     C*----------------------------------------------------*
071000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
071100111031     C*----------------------------------------------------*
071200111031     C     EXEVATB       BEGSR
071300111031     C*
071400130212     C                   EVAL      VATTRC='B'
071500130212     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
071600130212     C                   exsr      wrivat                                       => scarico VAT
071700111031     C*
071800111031     C                   ENDSR
071900111031     C*----------------------------------------------------*
072000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
072100111031     C*----------------------------------------------------*
072200111031     C     EXEVATE       BEGSR
072300111031     C*
072400111031     C                   EVAL      VATTRC='E'
072500111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
072600111031     C                   exsr      wrivat                                       => scarico VAT
072700111031     C*
072800111031     C                   ENDSR
072900120402     C*----------------------------------------------------*
073000120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
073100140325     C*----------------------------------------------------*
073200120402     C     EXEVATI       BEGSR
073300120402     C*
073400120402     C                   EVAL      VATTRC='I'
073500120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
073600120402     C                   exsr      wrivat                                       => scarico VAT
073700120402     C*
073800150917     C                   IF        %subst(vindta:663+35:15) <> *blank
073900120402     C                   EVAL      VATTRC='J'
074000150917     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
074100120402     C                   exsr      wrivat                                       => scarico VAT
074200150917     C                   ENDIF
074300120402     C*
074400120402     C                   ENDSR
074500111031     C*----------------------------------------------------*
074600111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
074700111031     C*----------------------------------------------------*
074800111031     C     WRIVAT        BEGSR
074900111031     C*
075000111031     C* Scrivo solo se valorizzato qualcosa
075100111031     C                   IF        VATNOT <> *blanks
075200111031     C                   WRITE     FIVAT000
075300111031     C                   ENDIF
075400111031     C*
075500111031     C                   ENDSR
075600020904
075700020904
075800010202     C*----------------------------------------------------*
075900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
076000010202     C*----------------------------------------------------*
076100100525     C     PREVASX       BEGSR
076200100525     C*
076300021113     C* Compongo il nome del membro da dare al FIVATWWR
076400010202     C                   eval      parmbr = vlrhdl
076500010202     C                   movel     'M'           parmbr
076600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
076700010202     C                   eval      paropz = '1'
076800100525     C*
076900010202     C* Effettuo la chiamata al CLLE preposto
077000100525     C  N51              call(e)   'TITVVTC'
077100010202     C                   parm                    parccm
077200010202     C                   parm                    parmbr
077300010202     C                   parm                    paropz
077400100525     C   51              call(e)   'TITVVBC'
077500100525     C                   parm                    parccm
077600100525     C                   parm                    parmbr
077700100525     C                   parm                    paropz
077800100525     C*
077900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
078000010202     C                   if        %error
078100010202     C                   movel     '1'           chkcall
078200010202     C                   else
078300010202     C                   movel     '0'           chkcall
078400010202     C                   endif
078500010202     C*
078600010202     C                   ENDSR
078700000801     C*----------------------------------------------------*
078800000801     C*  CONTROLLO NUMERICITA' CAMPI
078900000801     C*----------------------------------------------------*
079000000801     C     CHKNUM        BEGSR
079100081113     C*
079200081113     C                   IF        PiDecChr = *blanks
079300110408     C                   EVAL      PiDecChr = '.'
079400081113     C                   ENDIF
079500091223     C*
079600081113     C                   callp(e)  UBISNUM_Check(PiStr
079700081113     C                                          :PiDecChr
079800081113     C                                          :PiVal
079900081113     C                                          :PiNum
080000081113     C                                          :PiInt)
080100081113     C*
080200000801     C                   IF        %error
080300000801     C                   EVAL      PiInt=*off
080400000801     C                   ENDIF
080500000801     C*
080600000801     C                   ENDSR
080700000801     C***
080800011113
080900011113
081000000801
081100000801
081200990920      /TITLE Invio dei dati al punto operativo.
081300010202     C     invio         BEGSR
081400990920     C*
081500021113     C* 1° invio FIVAT
081600010201     C                   reset                   dscmz
081700110411     C                   move      vatfgs        cmzdst
081800021113     C                   eval      cmzfld = 'FIVATWWR'
081900010201     C                   eval      cmzmbd = vlrhdl
082000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082100021009     C***                if        prmfir = *blanks
082200021113     C                   eval      cmzfla = 'FIVAT00F'
082300021113     C                   eval      cmzmba = 'FIVAT00F'
082400021009     C***                else
082500021009     C***                eval      cmzfla = prmfir
082600021009     C***                eval      cmzmba = prmfir
082700021009     C***                endif
082800010201     C                   eval      cmznrr = *zeros
082900020305     C                   move      §ctrokvt      cmznrr
083000021018     C                   eval      cmzlba = vlrfl1
083100010201     C                   call(e)   'TIS711C'
083200010201     C                   parm                    dscmz
083300010201     C                   parm      *blanks       esito
083400010205     C                   if        %error
083500010205     C                             or cmzerr = '1'
083600010205     C                             or esito  = '1'
083700010205     C                   eval      wrkesito = '3'
083800010205     C                   else
083900010201     C*
084000021113     C* 2° invio FIVAB
084100100525     C                   if        *in51 = *off
084200010201     C                   reset                   dscmz
084300110411     C                   move      vabfgs        cmzdst
084400010201     C                   eval      cmzfld = vlrfou
084500010201     C                   eval      cmzmbd = vlrhdl
084600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
084700021009     C***                if        prmfir = *blanks
084800021113     C                   eval      cmzfla = 'FIVAB00F'
084900021113     C                   eval      cmzmba = 'FIVAB00F'
085000021009     C***                else
085100021009     C***                eval      cmzfla = prmfir
085200021009     C***                eval      cmzmba = prmfir
085300021009     C***                endif
085400010201     C                   eval      cmznrr = *zeros
085500010201     C                   move      §ctrokvb      cmznrr
085600021018     C                   eval      cmzlba = vlrfl1
085700010201     C                   call(e)   'TIS711C'
085800010201     C                   parm                    dscmz
085900010201     C                   parm      *blanks       esito
086000010201     C                   if        %error
086100010201     C                             or cmzerr = '1'
086200010201     C                             or esito  = '1'
086300010201     C                   eval      wrkesito = '3'
086400010201     C                   endif
086500100525     C                   endif
086600010205     C                   endif
086700990920     C*
086800000613     C                   ENDSR
086900000613     C***
087000070411
087100110627     C     *pssr         BEGSR
087200070411     C*
087300110627     C                   if        %open(tivin00r)
087400110627     C                   close     tivin00r
087500110627     C                   endif
087600110627     C                   if        %open(fivabwwr)
087700110627     C                   close     fivabwwr
087800110627     C                   endif
087900110627     C                   if        %open(fivatwwr)
088000110627     C                   close     fivatwwr
088100110627     C                   endif
088200110627     C*
088300110627     C* Effettuo la chiamata al CLLE preposto
088400110627     C  N51              call(e)   'TITVVTC'
088500110627     C                   parm                    parccm
088600110627     C                   parm                    parmbr
088700110627     C                   parm      '2'           paropz
088800110627     C   51              call(e)   'TITVVBC'
088900110627     C                   parm                    parccm
089000110627     C                   parm                    parmbr
089100110627     C                   parm      '2'           paropz
089200110627     C*
089300110627     C                   eval      wrkesito = '2'
089400110627     C*
089500110627     C                   seton                                        LR
089600110627     C*
089700110627     C                   ENDSR     '*CANCL'
089800070411     C***
089900070411
090000990910
090100000613     C     *inzsr        BEGSR
090200990910     C*
090300990910     C     *entry        plist
090400990920     C                   parm                    tivlrds
090500990921     C                   parm      wrkesito      esito
090600000724     C                   parm                    prmlit
090700000710     C                   parm                    prmfir
090800000613     C*
090900000830     C* CALCOLA LA DATA CORRENTE
091000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
091100091223     C                   eval      datcor = %dec(%date() : *ISO)
091200150917     C*
091300150917     C* Chiave su TABEL00F - parziale
091400150917     C     KEYtabP       KLIST
091500150917     C                   KFLD                    tblKUT
091600150917     C                   KFLD                    tblCOD
091700000830     C*
091800000613     C                   ENDSR
091900000613     C***
