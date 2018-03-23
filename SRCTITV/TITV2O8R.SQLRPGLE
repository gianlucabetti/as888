000100130322      /TITLE Upload via Internet: traduzione in FNVABWWR/TIVGD00F.
000200130322     H DECEDIT('0,') DATEDIT(*DMY.)
000300121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000400990908
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600130322     FTIVGD00F  O    E             DISK
000700130402     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000800130402     F                                     PREFIX(f_)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070730     D tisi95ds      e ds
001700130322     D fnvab00f      e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002600091223     D Num5_0          s              5  0
002700020725
002800020725     D*------------------
002900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003000020725     D*------------------
003100120327     D INPUT_DS        DS                  INZ
003200100729     D  VINDTA                     2048
003300100729     D  VINFLG                        1
003400130322     D  VINSPE                       10
003500100729     D  VINRRN                        8  0
003600020725     D*
003700091223
003800101119
003900101119     D*-------------------
004000101119     D* COSTANTI
004100101119     D*-------------------
004200101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
004300101119     D                                     èéù')
004400101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
004500101119     D                                     EEU')
004600081113
004700081113     D*------------------
004800081113     D* LINKING A DEFINIZIONI ESTERNE
004900081113     D*------------------
005000081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005100090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005200081113
005300990908
005400010201
005500010201
005600080117     C*
005700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005800080117     C
005900080117     C/EXEC SQL
006000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006100080117     C/END-EXEC
006200080117     C
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600000613     C*
006700130402     C                   EXSR      calprog
006800100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
006900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007000070730     C*
007100070730     C* Effettuo lancio TISI95 solo x chiusura
007200070730     C                   CLEAR                   TISI95DS
007300070730     C                   EVAL      I95TLA = 'C'
007400070730     C                   CALL      'TISI95R'
007500070730     C                   PARM                    TISI95DS
007600000616     C*
007700000801     C
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200130322     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR  *
008300910830     C*--------------------------------------------------------
008400070530     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900080117     C*
009000130322     C                   exsr      inzvar
009100130322     C                   exsr      defcam
009200130322     C                   exsr      wriINT
009300990910     C*
009400010201     C                   clear                   §CTROKVB          5 0
009500000801     C                   clear                   §CTRMO            5 0
009600000801     C                   clear                   §CTRNO            5 0
009700990910     C*
009800020725     C/EXEC SQL
009900020725     C+ declare C1 cursor for select
010000130322     C+ vindta, vinflg, substr(vindta, 9, 10) as sped, rrn(tivin00r)
010100060223     C+ from tivin00r
010200111020     C+ order by sped
010300020725     C+ for read only
010400020725     C/END-EXEC
010500020725     C
010600020725     C/EXEC SQL
010700020725     C+ open C1
010800020725     C/END-EXEC
010900020725     C
011000020725     C/EXEC SQL
011100070530     C+ Fetch C1 into :INPUT_DS
011200020725     C/END-EXEC
011300020725     C*
011400020725     C                   dow       sqlcod = *zeros
011500990913     C*
011600100310     C                   if        vindta > *blanks
011700000613     C                   add       1             rrnum
011800000801     C*
011900020725     C                   if        vinflg = *blanks
012000020725     C                             or vinflg = '0'
012100020725     C                             or vinflg = '2'
012200000801     C*
012300020725     C                   clear                   x_vinmsg
012400020725     C                   eval      x_vinflg = '1'
012500101119     C*
012600101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
012700101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
012800130322     C*
012900101119     C                   exsr      inzvar
013000101119     C                   exsr      defcam
013100071003     C                   exsr      impvab
013200100525     C                   exsr      wrivab
013300110201     C*
013400071009     C                   endif
013500000905     C*
013600000905     C                   else
013700020725     C                   eval      x_vinflg = '1'
013800000905     C                   endif
013900000905     C*
014000020725     C     VINRRN        chain     tivin000
014100020725     C                   if        %found(tivin00r)
014200020725     C                   eval      y_vinflg = x_vinflg
014300020725     C                   eval      y_vinmsg = x_vinmsg
014400020725     C                   update    tivin000
014500020725     C                   endif
014600020725     C*
014700020725     C/EXEC SQL
014800070530     C+ Fetch C1 into :INPUT_DS
014900020725     C/END-EXEC
015000020725     C*
015100020725     C                   enddo
015200020725     C*
015300020725     C/EXEC SQL
015400020725     C+ close C1
015500020725     C/END-EXEC
015600000905     C*
015700010202     C*
015800990910
015900990910     C* Se non ci sono record con errori ...
016000000710     C                   if        §ctrno = 0
016100990910     C* ... restituisco esito OK.
016200990921     C                   eval      wrkesito = '0'
016300990910     C                   else
016400130322     C                   if        §ctrokvb > 0
016500990921     C                   eval      wrkesito = '1'
016600000710     C                   else
016700000710     C                   eval      wrkesito = '2'
016800990910     C                   endif
016900000710     C                   endif
017000990910     C*
017100990914     C                   if        %open(tivin00r)
017200990908     C                   close     tivin00r
017300990914     C                   endif
017400990920     C*
017500910830     C                   ENDSR
017600000613     C***
017700130322
017800130322
017900130322
018000130322     C*----------------------------------------------------*
018100130322     C*  INSERIMENTO INTESTAZIONE COLONNE
018200130322     C*----------------------------------------------------*
018300130322     C     WRIINT        BEGSR
018400130322     C*
018500130322     C                   clear                   tivgd000
018600130322     C                   movel     ';'           wSep              1
018700130322     C                   eval      vgdDTA =
018800130322     C                                  'VABCCM'               + wSep +
018900130322     C                                  'VABCTR'               + wSep +
019000130322     C                                  'VABCBO'               + wSep +
019100130322     C                                  'VABRMA'               + wSep +
019200130322     C                                  'VABRMO'               + wSep +
019300130322     C                                  'VABRSD'               + wSep +
019400130322     C                                  'VABIND'               + wSep +
019500130322     C                                  'VABLOD'               + wSep +
019600130322     C                                  'VABPRD'               + wSep +
019700130322     C                                  'VABCAD'               + wSep +
019800130322     C                                  'VABNOT'               + wSep +
019900130322     C                                  'VABNT2'               + wSep +
020000130322     C                                  'VABTSP'               + wSep +
020100130322     C                                  'VABTC1'               + wSep +
020200130322     C                                  'VABTC2'               + wSep +
020300130322     C                                  'VABGC1'               + wSep +
020400130322     C                                  'VABRMN'               + wSep +
020500130322     C                                  'VABNCL'               + wSep +
020600130322     C                                  'VABPKB'               + wSep +
020700130322     C                                  'VABTIC'               + wSep +
020800130322     C                                  'VABCAS'               + wSep +
020900130322     C                                  'VABDCR'               + wSep +
021000130322     C                                  'VABTRC'
021100130322     C* Scarico buffer di output
021200130322     C                   exsr      wriVGD
021300130322     C*
021400130322     C                   ENDSR
021500100412
021600100412
021700010305
021800010305     C*----------------------------------------------------*
021900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022000010305     C*----------------------------------------------------*
022100020305     C     WRIVAB        BEGSR
022200100412     C*
022300100412     C* Gestisco l'eventuale rottura x numero spedizione
022400100412     C                   if        VABRMA = *blanks
022500100412     C                   movel(P)  VABRMN        VABRMA
022600100412     C                   endif
022700100412     C*
022800130322     C                   if        *in31 = *off
022900130322     C                   clear                   tivgd000
023000130322     C                   movel     ';'           wSep              1
023100130322     C                   eval      vgdDTA =
023200130322     C                             %trim(%editc(vabCCM:'X'))   + wSep +
023300130322     C                             %trim(%editc(vabCTR:'X'))   + wSep +
023400130322     C                             %trim(vabCBO)               + wSep +
023500130322     C                             %trim(vabRMA)               + wSep +
023600130322     C                             %trim(vabRMO)               + wSep +
023700130322     C                             %trim(vabRSD)               + wSep +
023800130322     C                             %trim(vabIND)               + wSep +
023900130322     C                             %trim(vabLOD)               + wSep +
024000130322     C                             %trim(vabPRD)               + wSep +
024100130322     C                             %trim(vabCAD)               + wSep +
024200130322     C                             %trim(vabNOT)               + wSep +
024300130322     C                             %trim(vabNT2)               + wSep +
024400130322     C                             %trim(vabTSP)               + wSep +
024500130322     C                             %trim(vabTC1)               + wSep +
024600130322     C                             %trim(vabTC2)               + wSep +
024700130322     C                             %trim(vabGC1)               + wSep +
024800130322     C                             %trim(%editc(vabRMN:'X'))   + wSep +
024900130322     C                             %trim(%editc(vabNCL:'X'))   + wSep +
025000130322     C                             %trim(%editc(vabPKB:'4'))   + wSep +
025100130322     C                             %trim(vabTIC)               + wSep +
025200130322     C                             %trim(%editc(vabCAS:'4'))   + wSep +
025300130322     C                             %trim(%editc(vabDCR:'X'))   + wSep +
025400130322     C                             %trim(wvabTRC)
025500130322     C* Scarico buffer di output
025600130322     C                   exsr      wriVGD
025700100525     C                   endif
025800010305     C*
025900010305     C                   ENDSR
026000000801     C*----------------------------------------------------*
026100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026200000801     C*----------------------------------------------------*
026300010201     C     INZVAR        BEGSR
026400000801     C*
026500010201     C                   Z-ADD     *zeros        Num5_0
026600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026700020725     C                   MOVEL     '0'           FlgCAS            1
026800000801     C*
026900000801     C                   ENDSR
027000000801     C*----------------------------------------------------*
027100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027200000801     C*----------------------------------------------------*
027300020904     C     DEFCAM        BEGSR
027400080923     C*
027500100928     C                   SETOFF                                       505160
027600000801     C*
027700130322     C                   CLEAR                   FNVAB00F
027800130322     C                   CLEAR                   TIVGD000
027900070730     C* Imposto i valori di default...
028000130322     C                   EVAL      VABCCM = 1311171
028100120327     C                   EVAL      VABLNP = 131
028200111110     C                   EVAL      VABCTR = 000
028300100525     C                   EVAL      VABCBO = '1'
028400070222     C* ... e poi verifico se sono stati passati come parametri
028500070222     C                   IF        vlrppt > *blanks
028600100525     C*
028700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
028800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028900070222     C                   EXSR      CHKNUM
029000070222     C                   IF        PiInt=*on
029100070222     C                   Z-ADD     PiVal         VABCCM
029200070222     C                   ENDIF
029300070222     C                   ENDIF
029400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABLNP
029900070222     C                   ENDIF
030000070222     C                   ENDIF
030100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030300070222     C                   EXSR      CHKNUM
030400070222     C                   IF        PiInt=*on
030500070222     C                   Z-ADD     PiVal         VABCTR
030600070222     C                   ENDIF
030700070928     C                   ENDIF
030800100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
030900100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
031000100525     C                   ENDIF
031100130328     C                   IF        %subst(vlrppt:18:1) <> *blanks
031200130328     C                   EVAL      VABTSP=%trim(%subst(vlrppt:18:1))
031300130328     C                   ENDIF
031400100525     C*
031500070222     C                   ENDIF
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032000000801     C*----------------------------------------------------*
032100100730     C     IMPVAB        BEGSR
032200070530     C*
032300070530     C                   SETOFF                                       3132
032400070928     C*
032500070928     C                   MOVEL     datcor        VABAAS
032600070928     C                   MOVE      datcor        VABMGS
032700130322     C*
032800130322     C* Forzatura campo "EasyWeb"
032900130322     C                   MOVEL     *blanks       wVABTRC          35
033000130322     C                   eval      wVABTRC = %trim(%subst(vindta:532:15))
033100100729     C*
033200100729     C* Reperimento campi ALFA
033300120327     C                   IF        %trim(%subst(vindta:27:1)) = 'F'
033400120327     C                   EVAL      VABCBO='1'
033500120327     C                   ENDIF
033600120327     C                   IF        %trim(%subst(vindta:27:1)) = 'A'
033700120327     C                   EVAL      VABCBO='2'
033800120327     C                   ENDIF
033900120327     C                   EVAL      VABRMA=%trim(%subst(vindta:29:16))
034000120515     C                   EVAL      VABRMO=%trim(%subst(vindta:52:22))
034100120327     C                   EVAL      VABRSD=%trim(%subst(vindta:159:30))
034200120327     C                   EVAL      VABIND=%trim(%subst(vindta:189:36))
034300120327     C                   EVAL      VABLOD=%trim(%subst(vindta:225:24))
034400120327     C                   EVAL      VABPRD=%trim(%subst(vindta:249:2))
034500120404     C                   EVAL      VABCAD=%trim(%subst(vindta:251:5))
034600120328     C                   EVAL      VABNOT=%trim(%subst(vindta:298:30))
034700120328     C                   EVAL      VABNT2=%trim(%subst(vindta:328:30))
034800130328     C                   EVAL      VABTSP='C'
034900120327     C                   IF        %trim(%subst(vindta:530:1)) = 'E'
035000120327     C                   EVAL      VABTSP='E'
035100120327     C                   ENDIF
035200120327     C                   EVAL      VABTC1=*blank
035300120327     C                   IF        %trim(%subst(vindta:531:1)) = 'A'
035400120327     C                   EVAL      VABTC1='A'
035500120327     C                   ENDIF
035600120327     C                   EVAL      VABTC2=*blank
035700120327     C                   IF        %trim(%subst(vindta:554:1)) = 'P'
035800120327     C                   EVAL      VABTC2='P'
035900120327     C                   ENDIF
036000120327     C                   EVAL      VABGC1=%trim(%subst(vindta:535:2))
036100100729     C*
036200100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
036300100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036400100729     C     '@':'A'       XLATE     VABRSD        VABRSD
036500100729     C* ==
036600100729     C*
036700100729     C* Reperimento campi NUMERICI
036800100729     C                   MOVEL     DATCOR        VABAAS
036900100729     C                   MOVE      DATCOR        VABMGS
037000110408     C*
037100111020     C* NSP
037200101119     C                   IF        *IN51 = *off
037300120327     C                   EVAL      PiStr=%subst(vindta:12:7)
037400100730     C                   EXSR      CHKNUM
037500100730     C                   IF        PiInt=*on
037600100928     C  N60              Z-ADD     PiVal         VABNSP
037700100730     C                   ELSE
037800100730     C                   SETON                                        32
037900100730     C                   Z-ADD     1             VABRMN
038000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038100130322     C                             + ' ' + 'VABNSP'
038200100730     C                   ENDIF
038300101119     C*
038400101119     C                   ELSE
038500101119     C*
038600120327     C                   EVAL      PiStr=%subst(vindta:12:7)
038700101119     C                   EXSR      CHKNUM
038800101119     C                   IF        PiInt=*on
038900101119     C                   ELSE
039000101119     C                   SETON                                        32
039100101119     C                   ENDIF
039200101119     C                   ENDIF
039300111020     C* RMN
039400120327     C                   EVAL      PiStr=%subst(vindta:12:7)
039500111020     C                   EXSR      CHKNUM
039600111020     C                   IF        PiInt=*on
039700111020     C                   Z-ADD     PiVal         VABRMN
039800111020     C                   ELSE
039900111020     C  N51              SETON                                        32
040000111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
040100111020     C                             + ' ' + 'VABRMN'
040200111020     C                   ENDIF
040300110801     C* NCL
040400120327     C                   EVAL      PiStr=%subst(vindta:259:5)
040500110801     C                   EXSR      CHKNUM
040600110801     C                   IF        PiInt=*on
040700120330     C                   Z-ADD     PiVal         VABNCL
040800110801     C                   ELSE
040900110801     C  N51              SETON                                        32
041000111020     C  N51              Z-ADD     *zeros        VABNCL
041100110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
041200110801     C                             + ' ' + 'VABNCL'
041300110801     C                   ENDIF
041400100729     C* PKB
041500120327     C                   EVAL      PiStr=%trim(%subst(vindta:270:7))
041600100729     C                   EXSR      CHKNUM
041700100729     C                   IF        PiNum=*on
041800120327     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
041900120330     C                   Z-ADD(H)  PiVal         VABPKB
042000100729     C                   ELSE
042100101119     C  N51              SETON                                        32
042200101119     C  N51              Z-ADD     *zeros        VABPKB
042300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
042400100729     C                             + ' ' + 'VABPKB'
042500100729     C                   ENDIF
042600120327     C* CAS
042700120515     C                   IF        %subst(vindta:396:11) <> *blanks AND
042800120515     C                             %subst(vindta:396:11) <> *all'0'
042900120327     C                   EVAL      PiStr=%trim(%subst(vindta:396:11))
043000130614     C                   EVAL      FlgCAS = '1'
043100120515     C                   EVAL      VABTIC='OM'
043200120327     C                   EXSR      CHKNUM
043300120327     C                   IF        PiNum=*on
043400120327     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
043500120330     C                   Z-ADD(H)  PiVal         VABCAS
043600120327     C                   ELSE
043700120327     C  N51              SETON                                        32
043800120327     C  N51              Z-ADD     *zeros        VABCAS
043900120327     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
044000120327     C                             + ' ' + 'VABCAS'
044100120327     C                   ENDIF
044200120515     C                   ENDIF
044300120327     C* DCR
044400120327     C* ipotizzo che la data che mi arriva sia in formato AAAAMMGG come il nostro,
044500120327     C* il POC non ha mandato esempi
044600120327     C                   EVAL      PiStr=%subst(vindta:522:8)
044700120327     C                   EXSR      CHKNUM
044800120327     C                   IF        PiInt=*on
044900120327     C                   Z-ADD     PiVal         VABDCR
045000120327     C                   ELSE
045100120327     C  N51              SETON                                        32
045200120327     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
045300120327     C                             + ' ' + 'VABDCR'
045400120327     C                   ENDIF
045500100729     C*
045600100729     C* Se provincia nn valorizzata la reperisco
045700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
045800100729     C                   IF        VABNZD  = *blanks AND
045900100729     C                             VABPRD  = *blanks AND
046000100729     C                             VABCAD <> *blanks
046100100729     C                   CLEAR                   TISI95DS
046200100729     C                   EVAL      I95TCN = '3'
046300100729     C                   Z-ADD     datcor        I95DAT
046400100729     C                   EVAL      I95NAR = VABNZD
046500100729     C                   EVAL      I95CAP = VABCAD
046600100729     C                   EVAL      I95LOC = VABLOD
046700100729     C                   CALL      'TISI95R'
046800100729     C                   PARM                    TISI95DS
046900100729     C                   EVAL      VABPRD = O95PRV
047000100729     C                   ENDIF
047100100729     C*
047200100729     C* Considerazioni finali su CBO/CAS
047300100729     C                   IF        FlgCAS = '1'
047400100729     C                   IF        VABCBO = '1'
047500100729     C                   EVAL      VABCBO = '4'
047600100729     C                   ENDIF
047700100729     C                   IF        VABCBO = '2'
047800100729     C                   EVAL      VABCBO = '6'
047900100729     C                   ENDIF
048000100729     C                   ENDIF
048100010202     C*
048200000801     C* Ebbene...
048300000801     C                   ADD       1             §CTRMO
048400070530     C                   IF        *in31 <> *zeros OR
048500070530     C                             *in32 <> *zeros
048600000801     C                   ADD       1             §CTRNO
048700020725     C                   EVAL      x_vinflg = '2'
048800000801     C                   ELSE
048900010201     C                   ADD       1             §CTROKVB
049000000801     C                   ENDIF
049100000801     C*
049200000801     C                   ENDSR
049300000801     C*----------------------------------------------------*
049400000801     C*  CONTROLLO NUMERICITA' CAMPI
049500000801     C*----------------------------------------------------*
049600000801     C     CHKNUM        BEGSR
049700081113     C*
049800081113     C                   IF        PiDecChr = *blanks
049900110408     C                   EVAL      PiDecChr = '.'
050000081113     C                   ENDIF
050100091223     C*
050200081113     C                   callp(e)  UBISNUM_Check(PiStr
050300081113     C                                          :PiDecChr
050400081113     C                                          :PiVal
050500081113     C                                          :PiNum
050600081113     C                                          :PiInt)
050700081113     C*
050800000801     C                   IF        %error
050900000801     C                   EVAL      PiInt=*off
051000000801     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300000801     C***
051400011113
051500130322
051600130322
051700130322      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
051800130322     C     wriVGD        BEGSR
051900130322     C*
052000130322     C                   eval      vgdTIP = 'FC'
052100130322     C                   eval      vgdKSU = vlrKSC
052200130322     C                   eval      vgdTSC = 'WW'
052300130322     C                   eval      vgdDAT = datcor
052400130402     C                   eval      vgdPRG = f_tis7prgf
052500130322     C                   eval      vgdPGM = 'TITV2O8R'
052600130322     C                   write     tivgd000
052700130322     C*
052800130322     C                   ENDSR
052900130322     C*------------------------------------------------------------------------*
053000130402
053100130402
053200130402
053300130402      /TITLE Valorizzazione Progressivo Applicazione
053400130402     C     calprog       begsr
053500130402     C*
053600130402     C                   movel     *blanks       dwlisv            2
053700130402     C                   movel     *all'0'       dwlprg           10
053800130402     C                   z-add     *zeros        wrkprg            8 0
053900130402     C*
054000130402     C                   eval      dwlisv = 'OE'
054100130402     C*
054200170324     C     *start        setll     tis7prgf
054300130402     C                   read(e)   tis7prgf
054400130402     C                   if        not %error
054500130402     C                   eval      dwlprg = f_tis7prgf
054600130402     C*
054700130402     C                   move(p)   dwlprg        wrkprg
054800130402     C                   add       1             wrkprg
054900130402     C                   move(p)   wrkprg        dwlprg
055000130402     C                   movel     dwlisv        dwlprg
055100130402     C*
055200130402     C                   eval      f_tis7prgf = dwlprg
055300130402     C                   update    tis7prg0
055400170324     C                   else
055500170324     C                   eval      f_tis7prgf = 'XX00000000'
055600130402     C                   endif
055700130402     C*
055800130402     C                   endsr
055900130322
056000130322
056100130322
056200070411
056300110627     C     *pssr         BEGSR
056400070411     C*
056500110627     C                   if        %open(tivin00r)
056600110627     C                   close     tivin00r
056700110627     C                   endif
056800110627     C*
056900110627     C                   eval      wrkesito = '2'
057000110627     C*
057100110627     C                   seton                                        LR
057200110627     C*
057300110627     C                   ENDSR     '*CANCL'
057400070411     C***
057500070411
057600990910
057700000613     C     *inzsr        BEGSR
057800990910     C*
057900990910     C     *entry        plist
058000990920     C                   parm                    tivlrds
058100990921     C                   parm      wrkesito      esito
058200000724     C                   parm                    prmlit
058300000710     C                   parm                    prmfir
058400000613     C*
058500000830     C* CALCOLA LA DATA CORRENTE
058600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058700091223     C                   eval      datcor = %dec(%date() : *ISO)
058800000830     C*
058900000613     C                   ENDSR
059000000613     C***
