000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300170801     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600170801     FTIVGD00F  O    E             DISK
000700170801     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000800170801     F                                     prefix(f_)
000900100412     FFIVABwwr  O    E             DISK    usropn
001000021113     FFIVATwwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900070730     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500170801     D depspe          s             10    INZ(*blanks)
002600170801     D curspe          s             10    INZ(*blanks)
002700010202     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100170801     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003200091223     D Num5_0          s              5  0
003300170801     D IdColloIn       s             15  0
003400170801     D wX              s              2  0
003500170801     D pptCHR          s              1    inz
003600170801     D pptCSV          s              1    inz
003700170801     D wBufferOut      s           2048    inz
003800170801     D wVGD_Head       s              1  0 inz(*zeros)
003900170801     D wVATNOT_A       s             35    inz
004000170801     D wVATNOT_B       s             35    inz
004100170801     D wVATNOT_E       s             35    inz
004200170801     D wVATNOT_I       s             70    inz
004300170801     D wVATNOT_J       s             70    inz
004400170801     D wVATNOT_S       s             35    inz
004500020725
004600020725     D*------------------
004700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004800020725     D*------------------
004900070530     D INPUT_DS        DS                  INZ
005000100729     D  VINDTA                     2048
005100100729     D  VINFLG                        1
005200120423     D  VINSPE                       10
005300100729     D  VINRRN                        8  0
005400020725     D*
005500081113
005600091223     D*------------------
005700091223     D* DS REPERIMENTO NUMERATORE
005800091223     D*------------------
005900100309     D trul33ds      e ds                  inz
006000091223     D*------------------
006100091223     D* DS ARCHITETTURA
006200091223     D*------------------
006300091223     D kpjba         e ds                  inz
006400091223
006500101119
006600101119     D*-------------------
006700101119     D* COSTANTI
006800101119     D*-------------------
006900101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
007000101119     D                                     èéù')
007100101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
007200101119     D                                     EEU')
007300081113
007400081113     D*------------------
007500081113     D* LINKING A DEFINIZIONI ESTERNE
007600081113     D*------------------
007700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007800090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007900081113
008000010201
008100010201
008200080117     C*
008300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008400080117     C
008500080117     C/EXEC SQL
008600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008700080117     C/END-EXEC
008800080117     C
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200000613     C*
009300170801     C                   exsr      defcam
009400170801     C   50              exsr      calprog
009500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009600170801     C   50              exsr      strvgd
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900170801     C                   call(e)   'TITVVTC'
010000100525     C                   parm                    parccm
010100100525     C                   parm                    parmbr
010200100525     C                   parm      '2'           paropz
010300070730     C*
010400070730     C* Effettuo lancio TISI95 solo x chiusura
010500070730     C                   CLEAR                   TISI95DS
010600070730     C                   EVAL      I95TLA = 'C'
010700070730     C                   CALL      'TISI95R'
010800070730     C                   PARM                    TISI95DS
010900000616     C*
011000000801     C
011100010201     C                   seton                                        LR
011200170801
011300170801
011400170801     C*--------------------------------------------------------
011500170801     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011600170801     C*--------------------------------------------------------
011700170801     C     STRVGD        BEGSR
011800170801     C*
011900170801     C
012000170801     C/EXEC SQL
012100170801     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012200170801     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012300170801     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012400170801     C+       VGDTSC = :VGDTSC
012500170801     C/END-EXEC
012600170801     C
012700170801     C*
012800170801     C                   ENDSR
012900170801     C***
013000170801
013100170801
013200000801
013300910830     C*--------------------------------------------------------
013400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
013500910830     C*--------------------------------------------------------
013600070530     C     RWFILE        BEGSR
013700990910     C*
013800990914     C                   if        not %open(tivin00r)
013900990908     C                   open      tivin00r
014000990914     C                   endif
014100070530     C*
014200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
014300100525     C                   exsr      prevasx
014400010201     C*
014500010202     C                   if        chkcall = '0'
014600010202     C*
014700100525     C                   if        not %open(fivabwwr)
014800100525     C                   open      fivabwwr
014900100525     C                   endif
015000100525     C*
015100021113     C                   if        not %open(fivatwwr)
015200021113     C                   open      fivatwwr
015300010201     C                   endif
015400080117     C*
015500080117     C                   EXSR      INZVAR
015600990910     C*
015700010201     C                   clear                   §CTROKVB          5 0
015800020305     C                   clear                   §CTROKVT          5 0
015900000801     C                   clear                   §CTRMO            5 0
016000000801     C                   clear                   §CTRNO            5 0
016100990910     C*
016200020725     C/EXEC SQL
016300020725     C+ declare C1 cursor for select
016400110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
016500060223     C+ from tivin00r
016600110201     C+ order by sped, substr(vindta, 108, 10) desc
016700020725     C+ for read only
016800020725     C/END-EXEC
016900020725     C
017000020725     C/EXEC SQL
017100020725     C+ open C1
017200020725     C/END-EXEC
017300020725     C
017400020725     C/EXEC SQL
017500070530     C+ Fetch C1 into :INPUT_DS
017600020725     C/END-EXEC
017700020725     C*
017800020725     C                   dow       sqlcod = *zeros
017900990913     C*
018000100310     C                   if        vindta > *blanks
018100000613     C                   add       1             rrnum
018200000801     C*
018300020725     C                   if        vinflg = *blanks
018400020725     C                             or vinflg = '0'
018500020725     C                             or vinflg = '2'
018600000801     C*
018700020725     C                   clear                   x_vinmsg
018800020725     C                   eval      x_vinflg = '1'
018900101119     C*
019000101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019100101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019200010305     C*
019300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019400110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
019500170801     C*
019600170801     C                   if        depspe <> curspe AND                         => rottura di spediz
019700170801     C                             depspe <> *blanks
019800170801     C                   exsr      repnsp
019900170801     C                   exsr      wrivab                                       => scarico spediz
020000170801     C                   endif
020100170801     C*
020200170801     C                   eval      depspe = curspe                              => memorizz. spediz
020300080117     C                   clear                   tivinds
020400020305     C                   exsr      impvab
020500170801     C  N50              exsr      wrivat_a                                     => carico VAT
020600170801     C  N50              exsr      wrivat_b                                     => carico VAT
020700170801     C  N50              exsr      wrivat_i                                     => carico VAT
020800170801     C  N50              exsr      wrivat_s                                     => carico VAT
020900170801     C  N50              exsr      wrivat_e                                     => carico VAT
021000110201     C*
021100071009     C                   endif
021200000905     C*
021300000905     C                   else
021400020725     C                   eval      x_vinflg = '1'
021500000905     C                   endif
021600000905     C*
021700020725     C     VINRRN        chain     tivin000
021800020725     C                   if        %found(tivin00r)
021900020725     C                   eval      y_vinflg = x_vinflg
022000020725     C                   eval      y_vinmsg = x_vinmsg
022100020725     C                   update    tivin000
022200020725     C                   endif
022300020725     C*
022400020725     C/EXEC SQL
022500070530     C+ Fetch C1 into :INPUT_DS
022600020725     C/END-EXEC
022700020725     C*
022800020725     C                   enddo
022900020725     C*
023000020725     C/EXEC SQL
023100020725     C+ close C1
023200020725     C/END-EXEC
023300170801     C*
023400170801     C* Scarico spedizioni in sospeso
023500170801     C                   exsr      wrivab
023600010202     C*
023700010202     C                   endif
023800990910
023900990910     C* Se non ci sono record con errori ...
024000000710     C                   if        §ctrno = 0
024100990910     C* ... restituisco esito OK.
024200990921     C                   eval      wrkesito = '0'
024300990910     C                   else
024400100525     C                   if        §ctrokvb > 0 OR
024500100525     C                             §ctrokvt > 0
024600990921     C                   eval      wrkesito = '1'
024700000710     C                   else
024800000710     C                   eval      wrkesito = '2'
024900990910     C                   endif
025000000710     C                   endif
025100990910     C*
025200990914     C                   if        %open(tivin00r)
025300990908     C                   close     tivin00r
025400990914     C                   endif
025500021113     C                   if        %open(fivabwwr)
025600021113     C                   close     fivabwwr
025700990914     C                   endif
025800021113     C                   if        %open(fivatwwr)
025900021113     C                   close     fivatwwr
026000010201     C                   endif
026100990910     C*
026200100525     C                   if        §ctrokvb > 0 OR
026300100525     C                             §ctrokvt > 0
026400000724     C                             and vlrpoi <> *zeros
026500010202     C                   exsr      invio
026600990920     C                   endif
026700990920     C*
026800910830     C                   ENDSR
026900000613     C***
027000100412
027100100412
027200010305
027300010305     C*----------------------------------------------------*
027400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027500010305     C*----------------------------------------------------*
027600020305     C     WRIVAB        BEGSR
027700170801     C*
027800170801     C* Verifiche sulla bolla
027900170801     C                   if        vabRMN = *zeros
028000170801     C                   eval      vabRMN = vabNSP
028100170801     C                   endif
028200170801     C                   if        vabCAS > *zeros AND vabVCA = *blanks
028300170801     C                   eval      vabVCA = 'EUR'
028400170801     C                   endif
028500170801     C*
028600170801     C   50              exsr      wriVAB_VGD
028700170801     C  N50              exsr      wriVAB_WW
028800010305     C*
028900010305     C                   ENDSR
029000170801
029100170801
029200170801     C*----------------------------------------------------*
029300170801     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
029400170801     C*----------------------------------------------------*
029500170801     C     WRIVAB_WW     BEGSR
029600170801     C*
029700170801     C                   if        not *in31
029800170801     C                   write     fivab000                                     => scarico il VAB
029900170801     C                   endif
030000170801     C*
030100170801     C                   ENDSR
030200170801
030300990920
030400000801     C*----------------------------------------------------*
030500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600000801     C*----------------------------------------------------*
030700010201     C     INZVAR        BEGSR
030800000801     C*
030900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031000020725     C                   MOVEL     '0'           FlgCAS            1
031100170801     C*
031200170801     C                   CLEAR                   FIVAB000
031300170801     C                   CLEAR                   FIVAT000
031400170801     C*
031500170801     C                   Z-ADD     *zeros        Num5_0
031600170801     C                   CLEAR                   wVATNOT_A
031700170801     C                   CLEAR                   wVATNOT_B
031800170801     C                   CLEAR                   wVATNOT_E
031900170801     C                   CLEAR                   wVATNOT_I
032000170801     C                   CLEAR                   wVATNOT_J
032100170801     C                   CLEAR                   wVATNOT_S
032200170801     C*
032300170801     C                   EXSR      DEFCAM
032400000801     C*
032500000801     C                   ENDSR
032600000801     C*----------------------------------------------------*
032700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032800000801     C*----------------------------------------------------*
032900020904     C     DEFCAM        BEGSR
033000170801     C*
033100070730     C* Imposto i valori di default...
033200170801     C                   EVAL      VABCCM = 0221545
033300170801     C                   EVAL      VATCCM = 0221545
033400170801     C                   EVAL      VABLNP = 022
033500170801     C                   EVAL      VATLNP = 022
033600110128     C                   EVAL      VABCTR = 000
033700100525     C                   EVAL      VABCBO = '1'
033800170801     C*
033900070222     C* ... e poi verifico se sono stati passati come parametri
034000070222     C                   IF        vlrppt > *blanks
034100170801     C*
034200170801     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
034300170801     C                   IF        %subst(vlrppt:1:1) = 'S'
034400170801     C                   SETON                                        50        * OUT => Download
034500170801     C                   ELSE
034600170801     C                   SETOFF                                       50        * OUT => File WW
034700170801     C                   ENDIF
034800170801     C*
034900170801     C                   IF        %subst(vlrppt:2:1) <> *blanks
035000170801     C                   EVAL      pptCSV = %subst(vlrppt:2:1)
035100170801     C                   ELSE
035200170801     C                   EVAL      pptCSV = ';'
035300170801     C                   ENDIF
035400170801     C                   IF        %subst(vlrppt:3:1) <> *blanks
035500170801     C                   EVAL      pptCHR = %subst(vlrppt:3:1)
035600170801     C                   ELSE
035700170801     C                   EVAL      pptCHR = ' '
035800170801     C                   ENDIF
035900100525     C*
036000170801     C                   IF        %subst(vlrppt:4:7) <> *blanks
036100170801     C                   EVAL      PiStr=%trim(%subst(vlrppt:4:7))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCCM
036500070222     C                   Z-ADD     PiVal         VATCCM
036600070222     C                   ENDIF
036700070222     C                   ENDIF
036800170801     C                   IF        %subst(vlrppt:11:3) <> *blanks
036900170801     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037000070222     C                   EXSR      CHKNUM
037100070222     C                   IF        PiInt=*on
037200070222     C                   Z-ADD     PiVal         VABLNP
037300070222     C                   Z-ADD     PiVal         VATLNP
037400070222     C                   ENDIF
037500070222     C                   ENDIF
037600170801     C                   IF        %subst(vlrppt:14:3) <> *blanks
037700170801     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
037800070222     C                   EXSR      CHKNUM
037900070222     C                   IF        PiInt=*on
038000070222     C                   Z-ADD     PiVal         VABCTR
038100070222     C                   ENDIF
038200070928     C                   ENDIF
038300170801     C                   IF        %subst(vlrppt:17:2) <> *blanks
038400170801     C                   EVAL      VABCTM=%trim(%subst(vlrppt:17:2))
038500100525     C                   ENDIF
038600100525     C*
038700070222     C                   ENDIF
038800000801     C*
038900000801     C                   ENDSR
039000091223     C*----------------------------------------------------*
039100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039200091223     C*----------------------------------------------------*
039300091223     C     REPNSP        BEGSR
039400091223     C*
039500091223     C* NSP => Stacco un numeratore da AZNUM
039600091223     C                   clear                   TRUL33DS
039700091223     C                   eval      I33OPE = *zeros
039800091223     C                   eval      I33CNU = 302
039900091223     C                   eval      I33NUM = 1
040000091223     C                   movel     TRUL33DS      KPJBU
040100091223     C                   call      'TRUL33R'
040200091223     C                   parm                    KPJBA
040300091223     C                   movel     KPJBU         TRUL33DS
040400091223     C                   if        O33ERR = *zeros
040500091223     C                   z-add     O33NRF        VABNSP
040600091223     C                   z-add     O33NRF        VATNSP
040700091223     C                   else
040800091223     C                   SETON                                        31
040900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000091223     C                             + ' ' + 'VABNSP VATNSP'
041100091223     C                   endif
041200091223     C*
041300091223     C                   ENDSR
041400000801     C*----------------------------------------------------*
041500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041600000801     C*----------------------------------------------------*
041700100730     C     IMPVAB        BEGSR
041800070530     C*
041900070530     C                   SETOFF                                       3132
042000070928     C*
042100070928     C                   MOVE(P)   vlrpoi        VABFGS
042200070928     C                   MOVE(P)   vlrpoi        VATFGS
042300070928     C*
042400070928     C                   MOVEL     datcor        VABAAS
042500070928     C                   MOVEL     datcor        VATAAS
042600070928     C                   MOVE      datcor        VABMGS
042700100729     C*
042800100729     C* Reperimento campi ALFA
042900110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
043000110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043100110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043200110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
043300110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
043400150520     C                   EVAL      VABNZD=%trim(%subst(vindta:257:3))
043500150520     C                   IF        VABNZD = 'I' or
043600150520     C                             VABNZD = 'IT' or
043700150520     C                             VABNZD = 'ITA'
043800150520     C                   EVAL      VABNZD = *blank
043900150520     C                   ENDIF
044000110127     C*
044100110127     C                   SELECT
044200110201     C                   WHEN      %subst(vindta:207:1) = 'F'
044300110201     C                   EVAL      VABCBO = '1'
044400110201     C                   WHEN      %subst(vindta:207:1) = 'A'
044500110201     C                   EVAL      VABCBO = '2'
044600140725     C                   ENDSL
044700150528     C*
044800150528     C                   SELECT
044900150528     C                   WHEN      %subst(vindta:263:1) = 'D'
045000150528     C                   EVAL      VABCTM = '7R'
045100150528     C                   WHEN      %subst(vindta:263:1) = 'E'
045200150528     C                   EVAL      VABCTM = '7Q'
045300150528     C                   WHEN      %subst(vindta:263:1) = 'F'
045400150528     C                   EVAL      VABCTM = '7Q'
045500150528     C                   WHEN      %subst(vindta:263:1) = *BLANK
045600150528     C                   EVAL      VABCTM = '7Q'
045700150528     C                   OTHER
045800150528     C                   EVAL      VABCTM = '7Q'
045900150528     C                   ENDSL
046000110127     C*
046100140725     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
046200170801     C                   MOVEL     *blanks       wNOTE            70
046300170801     C                   EVAL      wNOTE=%trim(%subst(vindta:968:50))
046400170801     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
046500170801     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
046600100729     C*
046700100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
046800100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046900100729     C     '@':'A'       XLATE     VABRSD        VABRSD
047000100729     C* ==
047100100729     C*
047200100729     C* Reperimento campi NUMERICI
047300100729     C                   MOVEL     DATCOR        VABAAS
047400100729     C                   MOVE      DATCOR        VABMGS
047500110201     C* NCL
047600110201     C                   z-add     *zeros        wCOLLI            5 0
047700110201     C                   z-add     *zeros        wBANCALI          5 0
047800110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
047900100928     C                   EXSR      CHKNUM
048000100928     C                   IF        PiInt=*on
048100110201     C                   Z-ADD     PiVal         wCOLLI
048200170801     C                   ELSE
048300170801     C                   SETON                                        32
048400170801     C                   Z-ADD     *zeros        VABNCL
048500170801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048600100928     C                             + ' ' + 'VABNCL'
048700100928     C                   ENDIF
048800110201     C*
048900110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
049000100729     C* PKB
049100110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
049200100729     C                   EXSR      CHKNUM
049300100729     C                   IF        PiNum=*on
049400110201     C                   Z-ADD     PiVal         VABPKB
049500170801     C                   ELSE
049600170801     C                   SETON                                        32
049700170801     C                   Z-ADD     *zeros        VABPKB
049800170801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049900100729     C                             + ' ' + 'VABPKB'
050000100729     C                   ENDIF
050100110201     C* VLB
050200170801     C                   IF        %trim(%subst(vindta:234:11)) <> '0,0' AND
050300170801     C                             %trim(%subst(vindta:234:11)) <> '000000000,0'
050400140725     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
050500140725     C                   EXSR      CHKNUM
050600140725     C                   IF        PiNum=*on
050700140725     C                   Z-ADD(H)  PiVal         VABVLB
050800140725     C                   ELSE
050900170801     C                   SETON                                        32
051000170801     C                   Z-ADD     *zeros        VABVLB
051100170801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200140725     C                             + ' ' + 'VABVLB'
051300140725     C                   ENDIF
051400140725     C                   ENDIF
051500100729     C* CAS
051600140725     C                   IF        %subst(vindta:137:10)<> *blanks      AND
051700140725     C                             %subst(vindta:137:10)<> *zeros       AND
051800140725     C                             %subst(vindta:137:10)<> '0000000,00' AND
051900140725     C                             %trim(%subst(vindta:137:10))<> '0,00'
052000100729     C                   EVAL      FlgCAS = '1'
052100110127     C                   EVAL      VABVCA = 'EUR'
052200140725     C                   EVAL      VABTIC=%trim(%subst(vindta:147:40))
052300110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
052400100729     C                   EXSR      CHKNUM
052500100729     C                   IF        PiNum=*on
052600100928     C                   Z-ADD(H)  PiVal         VABCAS
052700100729     C                   ELSE
052800170801     C                   SETON                                        32
052900170801     C                   Z-ADD     *zeros        VABCAS
053000170801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053100100729     C                             + ' ' + 'VABCAS'
053200100729     C                   ENDIF
053300101119     C                   ELSE
053400101119     C                   EVAL      FlgCAS = '0'
053500100729     C                   ENDIF
053600110201     C* IAS
053700140725     C                   IF        %subst(vindta:223:11)<> *blanks       AND
053800140725     C                             %subst(vindta:223:11)<> *zeros        AND
053900140725     C                             %subst(vindta:223:11)<> '00000000,00' AND
054000140725     C                             %trim(%subst(vindta:223:11))<> '0,00'
054100110201     C                   EVAL      VABVAS = 'EUR'
054200110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
054300110201     C                   EXSR      CHKNUM
054400110201     C                   IF        PiNum=*on
054500110201     C                   Z-ADD(H)  PiVal         VABIAS
054600110201     C                   ELSE
054700170801     C                   SETON                                        32
054800170801     C                   Z-ADD     *zeros        VABIAS
054900170801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055000110201     C                             + ' ' + 'VABIAS'
055100110201     C                   ENDIF
055200110201     C                   ENDIF
055300100729     C*
055400100729     C* Se provincia nn valorizzata la reperisco
055500100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
055600100729     C                   IF        VABNZD  = *blanks AND
055700100729     C                             VABPRD  = *blanks AND
055800100729     C                             VABCAD <> *blanks
055900100729     C                   CLEAR                   TISI95DS
056000100729     C                   EVAL      I95TCN = '3'
056100100729     C                   Z-ADD     datcor        I95DAT
056200100729     C                   EVAL      I95NAR = VABNZD
056300100729     C                   EVAL      I95CAP = VABCAD
056400100729     C                   EVAL      I95LOC = VABLOD
056500100729     C                   CALL      'TISI95R'
056600100729     C                   PARM                    TISI95DS
056700100729     C                   EVAL      VABPRD = O95PRV
056800100729     C                   ENDIF
056900170801     C* VATNOT_A
057000170801     C***                EVAL      wVATNOT_A = %trim(%subst(vindta:1008:10))
057100170801     C* VATNOT_B
057200170801     C                   EVAL      wVATNOT_B = %trim(%subst(vindta:1038:12))
057300100729     C*
057400100729     C* Considerazioni finali su CBO/CAS
057500100729     C                   IF        FlgCAS = '1'
057600100729     C                   IF        VABCBO = '1'
057700100729     C                   EVAL      VABCBO = '4'
057800100729     C                   ENDIF
057900100729     C                   IF        VABCBO = '2'
058000100729     C                   EVAL      VABCBO = '6'
058100100729     C                   ENDIF
058200100729     C                   ENDIF
058300010202     C*
058400000801     C* Ebbene...
058500000801     C                   ADD       1             §CTRMO
058600070530     C                   IF        *in31 <> *zeros OR
058700070530     C                             *in32 <> *zeros
058800000801     C                   ADD       1             §CTRNO
058900020725     C                   EVAL      x_vinflg = '2'
059000000801     C                   ELSE
059100010201     C                   ADD       1             §CTROKVB
059200000801     C                   ENDIF
059300000801     C*
059400000801     C                   ENDSR
059500100127     C*----------------------------------------------------*
059600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059700100127     C*----------------------------------------------------*
059800100127     C     WRIVAT_A      BEGSR
059900100127     C*
060000100127     C* Valorizzo l buffer di scrittura del FIVAT
060100100127     C* - TIPO RECORD "A"
060200170801     C***                eval      VATTRC = 'A'
060300170801     C***                eval      VATNOT = wVATNOT_A
060400170801     C***                exsr      wrivat
060500100127     C*
060600100127     C                   ENDSR
060700010201     C*----------------------------------------------------*
060800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060900010201     C*----------------------------------------------------*
061000071003     C     WRIVAT_B      BEGSR
061100010201     C*
061200021113     C* Valorizzo l buffer di scrittura del FIVAT
061300070928     C* - TIPO RECORD "B"
061400170801     C                   eval      VATTRC = 'B'
061500170801     C                   eval      VATNOT = wVATNOT_B
061600170801     C                   exsr      wrivat
061700010201     C*
061800010201     C                   ENDSR
061900140725     C*----------------------------------------------------*
062000140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
062100140725     C*----------------------------------------------------*
062200140725     C     WRIVAT_I      BEGSR
062300140725     C*
062400140725     C* Valorizzo l buffer di scrittura del FIVAT
062500140725     C* - TIPO RECORD "I" / "J"
062600140728     C***                movel     *blanks       wTRC             70
062700140728     C***                eval      wTRC   = %trim(%subst(vindta:938:70))
062800140728     C***                eval      VATTRC = 'I'
062900140728     C***                eval      VATNOT = %subst(wTRC:01:35)
063000140728     C***                exsr      wrivat
063100140728     C***                eval      VATTRC = 'J'
063200140728     C***                eval      VATNOT = %subst(wTRC:36:35)
063300140728     C***                exsr      wrivat
063400140725     C*
063500140725     C                   ENDSR
063600140725     C*----------------------------------------------------*
063700140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
063800140725     C*----------------------------------------------------*
063900140725     C     WRIVAT_S      BEGSR
064000140725     C*
064100140725     C* Valorizzo l buffer di scrittura del FIVAT
064200140725     C* - TIPO RECORD "S"
064300170801     C***                eval      VATTRC = 'S'
064400170801     C***                eval      VATNOT = %trim(%subst(vindta:1018:10))
064500170801     C***                exsr      wrivat
064600140725     C*
064700140725     C                   ENDSR
064800071003     C*----------------------------------------------------*
064900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
065000071003     C*----------------------------------------------------*
065100071003     C     WRIVAT_E      BEGSR
065200071003     C*
065300071003     C* Valorizzo l buffer di scrittura del FIVAT
065400071003     C* - TIPO RECORD "E"
065500100729     C*
065600130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
065700130911     C*
065800130911     C*
065900100928     C                   eval      VATTRC = 'E'
066000130911     C*
066100141030     C     1             DO        VABNCL        wX
066200130911     C                   eval      IdColloIn = IdColloIn + 1
066300141030     C                   EVAL      VATNOT = 'CC' +
066400141030     C                              %editc(%dec(%subst(vindta:897:7):7:0):'X') +
066500141030     C                              %editc(%dec(%subst(vindta:118:2):2:0):'X') +
066600141030     C                              %editc(%dec(%subst(vindta:108:8):8:0):'X') +
066700141030     C                              %editc(wX:'X')
066800130911     C*
066900100928     C                   exsr      wrivat
067000130911     C                   ENDDO
067100071003     C*
067200071003     C                   ENDSR
067300100127     C*----------------------------------------------------*
067400100127     C*  SCARICO BUFFER FIVAT
067500100127     C*----------------------------------------------------*
067600100127     C     WRIVAT        BEGSR
067700100127     C*
067800100127     C                   if        vatNOT <> *blanks
067900100127     C                   ADD       1             §CTROKVT
068000100127     C                   write     FIVAT000
068100100127     C                   endif
068200100127     C*
068300100127     C                   ENDSR
068400020904
068500020904
068600010202     C*----------------------------------------------------*
068700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
068800010202     C*----------------------------------------------------*
068900100525     C     PREVASX       BEGSR
069000100525     C*
069100021113     C* Compongo il nome del membro da dare al FIVATWWR
069200010202     C                   eval      parmbr = vlrhdl
069300010202     C                   movel     'M'           parmbr
069400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069500010202     C                   eval      paropz = '1'
069600100525     C*
069700010202     C* Effettuo la chiamata al CLLE preposto
069800170801     C                   call(e)   'TITVVTC'
069900010202     C                   parm                    parccm
070000010202     C                   parm                    parmbr
070100010202     C                   parm                    paropz
070200100525     C*
070300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070400010202     C                   if        %error
070500010202     C                   movel     '1'           chkcall
070600010202     C                   else
070700010202     C                   movel     '0'           chkcall
070800010202     C                   endif
070900010202     C*
071000010202     C                   ENDSR
071100000801     C*----------------------------------------------------*
071200000801     C*  CONTROLLO NUMERICITA' CAMPI
071300000801     C*----------------------------------------------------*
071400000801     C     CHKNUM        BEGSR
071500081113     C*
071600081113     C                   IF        PiDecChr = *blanks
071700110201     C                   EVAL      PiDecChr = ','
071800081113     C                   ENDIF
071900091223     C*
072000081113     C                   callp(e)  UBISNUM_Check(PiStr
072100081113     C                                          :PiDecChr
072200081113     C                                          :PiVal
072300081113     C                                          :PiNum
072400081113     C                                          :PiInt)
072500081113     C*
072600000801     C                   IF        %error
072700000801     C                   EVAL      PiInt=*off
072800000801     C                   ENDIF
072900000801     C*
073000000801     C                   ENDSR
073100000801     C***
073200011113
073300000801
073400000801
073500990920      /TITLE Invio dei dati al punto operativo.
073600010202     C     invio         BEGSR
073700990920     C*
073800021113     C* 1° invio FIVAT
073900010201     C                   reset                   dscmz
074000010201     C                   move      vlrpoi        cmzdst
074100021113     C                   eval      cmzfld = 'FIVATWWR'
074200010201     C                   eval      cmzmbd = vlrhdl
074300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074400021009     C***                if        prmfir = *blanks
074500021113     C                   eval      cmzfla = 'FIVAT00F'
074600021113     C                   eval      cmzmba = 'FIVAT00F'
074700021009     C***                else
074800021009     C***                eval      cmzfla = prmfir
074900021009     C***                eval      cmzmba = prmfir
075000021009     C***                endif
075100010201     C                   eval      cmznrr = *zeros
075200020305     C                   move      §ctrokvt      cmznrr
075300021018     C                   eval      cmzlba = vlrfl1
075400010201     C                   call(e)   'TIS711C'
075500010201     C                   parm                    dscmz
075600010201     C                   parm      *blanks       esito
075700010205     C                   if        %error
075800010205     C                             or cmzerr = '1'
075900010205     C                             or esito  = '1'
076000010205     C                   eval      wrkesito = '3'
076100010205     C                   else
076200010201     C*
076300021113     C* 2° invio FIVAB
076400010201     C                   reset                   dscmz
076500010201     C                   move      vlrpoi        cmzdst
076600010201     C                   eval      cmzfld = vlrfou
076700010201     C                   eval      cmzmbd = vlrhdl
076800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076900021009     C***                if        prmfir = *blanks
077000021113     C                   eval      cmzfla = 'FIVAB00F'
077100021113     C                   eval      cmzmba = 'FIVAB00F'
077200021009     C***                else
077300021009     C***                eval      cmzfla = prmfir
077400021009     C***                eval      cmzmba = prmfir
077500021009     C***                endif
077600010201     C                   eval      cmznrr = *zeros
077700010201     C                   move      §ctrokvb      cmznrr
077800021018     C                   eval      cmzlba = vlrfl1
077900010201     C                   call(e)   'TIS711C'
078000010201     C                   parm                    dscmz
078100010201     C                   parm      *blanks       esito
078200010201     C                   if        %error
078300010201     C                             or cmzerr = '1'
078400010201     C                             or esito  = '1'
078500010201     C                   eval      wrkesito = '3'
078600010201     C                   endif
078700010205     C                   endif
078800990920     C*
078900000613     C                   ENDSR
079000000613     C***
079100170801
079200170801
079300170801      /TITLE Valorizzazione Progressivo Applicazione
079400170801     C     calprog       begsr
079500170801     C*
079600170801     C                   movel     *blanks       dwlisv            2
079700170801     C                   movel     *all'0'       dwlprg           10
079800170801     C                   z-add     *zeros        wrkprg            8 0
079900170801     C*
080000170801     C                   eval      dwlisv = vlrisv
080100170801     C*
080200170801     C     *start        setll     tis7prgf
080300170801     C                   read(e)   tis7prgf
080400170801     C                   if        not %error
080500170801     C                   eval      dwlprg = f_tis7prgf
080600170801     C*
080700170801     C                   move(p)   dwlprg        wrkprg
080800170801     C                   add       1             wrkprg
080900170801     C                   move(p)   wrkprg        dwlprg
081000170801     C                   movel     dwlisv        dwlprg
081100170801     C*
081200170801     C                   eval      f_tis7prgf = dwlprg
081300170801     C                   update    tis7prg0
081400170801     C                   else
081500170801     C                   eval      f_tis7prgf = 'XX00000000'
081600170801     C                   endif
081700170801     C*
081800170801     C                   endsr
081900170801
082000170801
082100170801      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
082200170801     C     wriVAB_VGD    BEGSR
082300170801     C*
082400170801     C                   if        not *in31
082500170801     C*
082600170801     C* Se primo giro allora scrivo testata
082700170801     C                   if        wVGD_Head = *zeros
082800170801     C                   eval      wVGD_Head = 1
082900170801     C                   eval      wBufferOut =
083000170801     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
083100170801     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
083200170801     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
083300170801     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
083400170801     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
083500170801     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
083600170801     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
083700170801     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
083800170801     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
083900170801     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
084000170801     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
084100170801     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
084200170801     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
084300170801     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
084400170801     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
084500170801     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
084600170801     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
084700170801     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
084800170801     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
084900170801     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
085000170801     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
085100170801     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
085200170801     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
085300170801     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
085400170801     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
085500170801     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
085600170801     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
085700170801     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
085800170801     C                   exsr      wriVGD
085900170801     C                   endif
086000170801     C*
086100170801     C* Quindi scrivo dati spedizione
086200170801     C                   eval      wBufferOut =
086300170801     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
086400170801     C                                                    +%trim(pptCHR)+pptCSV+
086500170801     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
086600170801     C                                                    +%trim(pptCHR)+pptCSV+
086700170801     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
086800170801     C                                                    +%trim(pptCHR)+pptCSV+
086900170801     C                             %trim(pptCHR)+%trim(VABRSD)
087000170801     C                                                    +%trim(pptCHR)+pptCSV+
087100170801     C                             %trim(pptCHR)+%trim(VABRD2)
087200170801     C                                                    +%trim(pptCHR)+pptCSV+
087300170801     C                             %trim(pptCHR)+%trim(VABIND)
087400170801     C                                                    +%trim(pptCHR)+pptCSV+
087500170801     C                             %trim(pptCHR)+%trim(VABLOD)
087600170801     C                                                    +%trim(pptCHR)+pptCSV+
087700170801     C                             %trim(pptCHR)+%trim(VABCAD)
087800170801     C                                                    +%trim(pptCHR)+pptCSV+
087900170801     C                             %trim(pptCHR)+%trim(VABPRD)
088000170801     C                                                    +%trim(pptCHR)+pptCSV+
088100170801     C                             %trim(pptCHR)+%trim(VABCBO)
088200170801     C                                                    +%trim(pptCHR)+pptCSV+
088300170801     C                             %trim(pptCHR)+%trim(VABRMO)
088400170801     C                                                    +%trim(pptCHR)+pptCSV+
088500170801     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
088600170801     C                                                    +%trim(pptCHR)+pptCSV+
088700170801     C                             %trim(pptCHR)+%trim(VABRMA)
088800170801     C                                                    +%trim(pptCHR)+pptCSV+
088900170801     C                             %trim(pptCHR)+%trim(VABNAS)
089000170801     C                                                    +%trim(pptCHR)+pptCSV+
089100170801     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
089200170801     C                                                    +%trim(pptCHR)+pptCSV+
089300170801     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
089400170801     C                                                    +%trim(pptCHR)+pptCSV+
089500170801     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
089600170801     C                                                    +%trim(pptCHR)+pptCSV+
089700170801     C                             %trim(pptCHR)+%trim(VABVCA)
089800170801     C                                                    +%trim(pptCHR)+pptCSV+
089900170801     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
090000170801     C                                                    +%trim(pptCHR)+pptCSV+
090100170801     C                             %trim(pptCHR)+%trim(VABVAS)
090200170801     C                                                    +%trim(pptCHR)+pptCSV+
090300170801     C                             %trim(pptCHR)+%trim(VABFFD)
090400170801     C                                                    +%trim(pptCHR)+pptCSV+
090500170801     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
090600170801     C                                                    +%trim(pptCHR)+pptCSV+
090700170801     C                             %trim(pptCHR)+%trim(VABVAS)
090800170801     C                                                    +%trim(pptCHR)+pptCSV+
090900170801     C                             %trim(pptCHR)+%trim(VABNOT)
091000170801     C                                                    +%trim(pptCHR)+pptCSV+
091100170801     C                             %trim(pptCHR)+%trim(wVATNOT_A)
091200170801     C                                                    +%trim(pptCHR)+pptCSV+
091300170801     C                             %trim(pptCHR)+%trim(wVATNOT_B)
091400170801     C                                                    +%trim(pptCHR)+pptCSV+
091500170801     C                             %trim(pptCHR)+%trim(wVATNOT_I)
091600170801     C                                                    +%trim(pptCHR)+pptCSV+
091700170801     C                             %trim(pptCHR)+%trim(wVATNOT_S)
091800170801     C                                                    +%trim(pptCHR)
091900170801     C                   exsr      wriVGD
092000170801     C*
092100170801     C                   endif
092200170801     C*
092300170801     C                   ENDSR
092400170801     C*------------------------------------------------------------------------*
092500170801
092600170801
092700170801
092800170801      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
092900170801     C     wriVGD        BEGSR
093000170801     C*
093100170801     C                   clear                   tivgd000
093200170801     C                   eval      vgdDTA = wBufferOut
093300170801     C                   eval      vgdTIP = 'EB'
093400170801     C                   eval      vgdKSU = vlrKSC
093500170801     C                   eval      vgdTSC = 'WW'
093600170801     C                   eval      vgdDAT = datcor
093700170801     C                   eval      vgdPRG = f_tis7prgf
093800170801     C                   eval      vgdPGM = procname
093900170801     C                   eval      vgdSTO = '?'
094000170801     C                   write     tivgd000
094100170801     C*
094200170801     C                   ENDSR
094300170801     C*------------------------------------------------------------------------*
094400170801
094500170801
094600070411
094700070411     C     *pssr         BEGSR
094800070411     C*
094900070411     C                   if        %open(tivin00r)
095000070411     C                   close     tivin00r
095100070411     C                   endif
095200070411     C                   if        %open(fivabwwr)
095300070411     C                   close     fivabwwr
095400070411     C                   endif
095500070411     C                   if        %open(fivatwwr)
095600070411     C                   close     fivatwwr
095700070411     C                   endif
095800070411     C*
095900070411     C* Effettuo la chiamata al CLLE preposto
096000170801     C                   call(e)   'TITVVTC'
096100100525     C                   parm                    parccm
096200100525     C                   parm                    parmbr
096300100525     C                   parm      '2'           paropz
096400070411     C*
096500070411     C                   eval      wrkesito = '2'
096600070411     C*
096700070411     C                   seton                                        LR
096800070411     C*
096900070411     C                   ENDSR     '*CANCL'
097000070411     C***
097100070411
097200990910
097300000613     C     *inzsr        BEGSR
097400990910     C*
097500990910     C     *entry        plist
097600990920     C                   parm                    tivlrds
097700990921     C                   parm      wrkesito      esito
097800000724     C                   parm                    prmlit
097900000710     C                   parm                    prmfir
098000000613     C*
098100000830     C* CALCOLA LA DATA CORRENTE
098200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
098300091223     C                   eval      datcor = %dec(%date() : *ISO)
098400000830     C*
098500000613     C                   ENDSR
098600000613     C***
