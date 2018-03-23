000100100331      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120629      *
000300120629      * PARTICOLARITA':
000400120629      * Il nr.spedizione mi viene passato per cui asterisco la routine REPNSP per il disk C con
000500120629      * N rcd per spedizione.
000600120703      * Il riferimento numerico viene preso come il rif.num.alfanumerico ma partendo 2 char dopo per
000700120703      * saltare la parte alfabetica.
000800120703      * I colli vanno sommati riga per riga della stessa spedizione, ma ogni riga è composta da 1
000900120703      * collo.
001000120703      * Il peso non è quello indicato sul file ma <quello indicato su file>/<colli indicati su file>
001100120703      *
001200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
001300120307
001400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001500100331     FEDIVABwr  O    E             DISK    usropn
001600100331     FEDIVATwr  O    E             DISK    usropn
001700990908
001800000801     D*----------------------------------------------------
001900000801     D* DICHIARAZIOINE VARIABILI DI WRK
002000000801     D*----------------------------------------------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500070730     D tisi95ds      e ds
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100120420     D depspe          s              7    INZ(*blanks)
003200120420     D curspe          s              7    INZ(*blanks)
003300010202     D parccm          s              8    INZ(*blanks)
003400010202     D parmbr          s             10    INZ(*blanks)
003500010202     D paropz          s              1    INZ(*blanks)
003600010202     D chkcall         s              1    INZ(*blanks)
003700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003800091223     D Num5_0          s              5  0
003900120703     D wDivPKB         s                   like(VABNCL)
004000020725
004100020725     D*------------------
004200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004300020725     D*------------------
004400070530     D INPUT_DS        DS                  INZ
004500120411     D  VINDTA                     2048
004600120411     D  VINFLG                        1
004700120420     D  VINSPE                        7
004800120411     D  VINRRN                        8  0
004900020725     D*
005000081113
005100091223     D*------------------
005200091223     D* DS REPERIMENTO NUMERATORE
005300091223     D*------------------
005400100309     D trul33ds      e ds                  inz
005500091223     D*------------------
005600091223     D* DS ARCHITETTURA
005700091223     D*------------------
005800091223     D kpjba         e ds                  inz
005900091223
006000081113
006100120411     D*-------------------
006200120411     D* COSTANTI
006300120411     D*-------------------
006400120411     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006500120411     D                                     èéù')
006600120411     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006700120411     D                                     EEU')
006800120411
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
008900120411     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
009000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009100000613     C*
009200120411     C* Effettuo la chiamata al CLLE preposto
009300120411     C  N51              call(e)   'TITVEVTC'
009400120411     C                   parm                    parccm
009500120411     C                   parm                    parmbr
009600120411     C                   parm      '2'           paropz
009700120411     C   51              call(e)   'TITVEVBC'
009800120411     C                   parm                    parccm
009900120411     C                   parm                    parmbr
010000120411     C                   parm      '2'           paropz
010100070730     C*
010200070730     C* Effettuo lancio TISI95 solo x chiusura
010300070730     C                   CLEAR                   TISI95DS
010400070730     C                   EVAL      I95TLA = 'C'
010500070730     C                   CALL      'TISI95R'
010600070730     C                   PARM                    TISI95DS
010700000616     C*
010800000801     C
010900010201     C                   seton                                        LR
011000990908
011100000801
011200910830     C*--------------------------------------------------------
011300100331     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011400910830     C*--------------------------------------------------------
011500070530     C     RWFILE        BEGSR
011600990910     C*
011700990914     C                   if        not %open(tivin00r)
011800990908     C                   open      tivin00r
011900990914     C                   endif
012000070530     C*
012100100331     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012200120412     C                   exsr      prevasx
012300010201     C*
012400010202     C                   if        chkcall = '0'
012500120412     C*
012600120412     C                   if        not %open(edivabwr)
012700120412     C                   open      edivabwr
012800120412     C                   endif
012900010202     C*
013000100331     C                   if        not %open(edivatwr)
013100100331     C                   open      edivatwr
013200010201     C                   endif
013300080117     C*
013400080117     C                   EXSR      INZVAR
013500080117     C                   EXSR      DEFCAM
013600990910     C*
013700010201     C                   clear                   §CTROKVB          5 0
013800020305     C                   clear                   §CTROKVT          5 0
013900000801     C                   clear                   §CTRMO            5 0
014000000801     C                   clear                   §CTRNO            5 0
014100990910     C*
014200120420     C* considero solo i rcd che iniziano con A1
014300120420     C*
014400020725     C/EXEC SQL
014500020725     C+ declare C1 cursor for select
014600120629     C+ vindta, vinflg, substr(vindta, 241, 10) as sped, rrn(tivin00r)
014700060223     C+ from tivin00r
014800060223     C+ order by sped
014900020725     C+ for read only
015000020725     C/END-EXEC
015100020725     C
015200020725     C/EXEC SQL
015300020725     C+ open C1
015400020725     C/END-EXEC
015500020725     C
015600020725     C/EXEC SQL
015700070530     C+ Fetch C1 into :INPUT_DS
015800020725     C/END-EXEC
015900020725     C*
016000020725     C                   dow       sqlcod = *zeros
016100990913     C*
016200100310     C                   if        vindta > *blanks
016300000613     C                   add       1             rrnum
016400000801     C*
016500020725     C                   if        vinflg = *blanks
016600020725     C                             or vinflg = '0'
016700020725     C                             or vinflg = '2'
016800000801     C*
016900020725     C                   clear                   x_vinmsg
017000020725     C                   eval      x_vinflg = '1'
017100120411     C*
017200120411     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
017300120411     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017400010305     C*
017500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017600120629     C                   EVAL      curspe=%trim(%subst(vindta:241:10))
017700010305     C*
017800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017900080923     C                   if        *in50 = *off
018000120411     C                   exsr      inzvar
018100120411     C                   exsr      defcam
018200071003     C                   exsr      impvab
018300071003     C                   exsr      wrivab
018400100127     C                   exsr      wrivat_a                                     => carico VAT
018500071003     C                   exsr      wrivat_b                                     => carico VAT
018600120411     C   51              exsr      wrivat_e                                     => carico VAT
018700120420     C                   exsr      wrivat_i                                     => carico VAT
018800120420     C                   exsr      wrivat_j                                     => carico VAT
018900071003     C                   else
019000080923     C*
019100071009     C                   if        wDISK = 'D'
019200091223     C                   exsr      repNSP
019300071009     C                   exsr      impvab
019400071009     C                   exsr      wrivab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071009     C                   exsr      wrivat_b                                     => carico VAT
019700071009     C                   exsr      wrivat_e                                     => carico VAT
019800120420     C                   exsr      wrivat_i                                     => carico VAT
019900120420     C                   exsr      wrivat_j                                     => carico VAT
020000071009     C                   else
020100071009     C*
020200010305     C                   if        depspe = *blanks                             => 1° giro
020300010305     C                   eval      depspe = curspe                              => memorizz. spediz
020400080117     C                   clear                   tivinds
020500120629     C***                exsr      repNSP
020600020305     C                   exsr      impvab
020700100127     C                   exsr      wrivat_a                                     => carico VAT
020800071003     C                   exsr      wrivat_b                                     => carico VAT
020900071003     C   50              exsr      wrivat_e                                     => carico VAT
021000120420     C                   exsr      wrivat_i                                     => carico VAT
021100120420     C                   exsr      wrivat_j                                     => carico VAT
021200010305     C                   else
021300020725     C                   if        depspe <> curspe                             => rottura di spediz
021400010305     C                   eval      depspe = curspe                              => memorizz. spediz
021500070928     C                   exsr      wrivab
021600080117     C                   clear                   tivinds
021700120831     C* azzero i campi che si sommano per spedizione
021800120831     C                   clear                   vabncl
021900120831     C                   clear                   vabpkb
022000120831     C                   clear                   wDivPkb
022100120629     C***                exsr      repNSP
022200020305     C                   exsr      impvab
022300100127     C                   exsr      wrivat_a                                     => carico VAT
022400071003     C                   exsr      wrivat_b                                     => carico VAT
022500071003     C   50              exsr      wrivat_e                                     => carico VAT
022600120420     C                   exsr      wrivat_i                                     => carico VAT
022700120420     C                   exsr      wrivat_j                                     => carico VAT
022800020305     C                   else                                                   => x stessa spediz
022900120411     C*
023000120411     C* Se nn richiesta esclusione spedizioni "duplicate"
023100120411     C                   select
023200120411     C                   when      wDUPREC = ' '
023300120411     C                   exsr      impvab
023400120411     C                   exsr      wrivat_a                                     => carico VAT
023500120411     C                   exsr      wrivat_b                                     => carico VAT
023600120411     C   50              exsr      wrivat_e                                     => carico VAT
023700120420     C                   exsr      wrivat_i                                     => carico VAT
023800120420     C                   exsr      wrivat_j                                     => carico VAT
023900120411     C                   when      wDUPREC = 'C'
024000120411     C   50              exsr      wrivat_e                                     => carico VAT
024100120411     C                   endsl
024200010305     C                   endif
024300010305     C                   endif
024400010305     C                   endif
024500071003     C                   endif
024600120411     C*
024700071009     C                   endif
024800000905     C*
024900000905     C                   else
025000020725     C                   eval      x_vinflg = '1'
025100000905     C                   endif
025200000905     C*
025300020725     C     VINRRN        chain     tivin000
025400020725     C                   if        %found(tivin00r)
025500020725     C                   eval      y_vinflg = x_vinflg
025600020725     C                   eval      y_vinmsg = x_vinmsg
025700020725     C                   update    tivin000
025800020725     C                   endif
025900020725     C*
026000020725     C/EXEC SQL
026100070530     C+ Fetch C1 into :INPUT_DS
026200020725     C/END-EXEC
026300020725     C*
026400020725     C                   enddo
026500020725     C*
026600020725     C/EXEC SQL
026700020725     C+ close C1
026800020725     C/END-EXEC
026900000905     C*
027000020305     C* Scarico i VAB rimasti "in sospeso"
027100071009     C                   if        wDISK = 'C'
027200071009     C                   exsr      wrivab
027300071009     C                   endif
027400010202     C*
027500010202     C                   endif
027600990910
027700990910     C* Se non ci sono record con errori ...
027800000710     C                   if        §ctrno = 0
027900990910     C* ... restituisco esito OK.
028000990921     C                   eval      wrkesito = '0'
028100990910     C                   else
028200120412     C                   if        §ctrokvb > 0 OR
028300120412     C                             §ctrokvt > 0
028400990921     C                   eval      wrkesito = '1'
028500000710     C                   else
028600000710     C                   eval      wrkesito = '2'
028700990910     C                   endif
028800000710     C                   endif
028900990910     C*
029000990914     C                   if        %open(tivin00r)
029100990908     C                   close     tivin00r
029200990914     C                   endif
029300100331     C                   if        %open(edivabwr)
029400100331     C                   close     edivabwr
029500990914     C                   endif
029600100331     C                   if        %open(edivatwr)
029700100331     C                   close     edivatwr
029800010201     C                   endif
029900990910     C*
030000120412     C                   if        (§ctrokvb > 0 OR
030100120412     C                              §ctrokvt > 0    )
030200000724     C                             and vlrpoi <> *zeros
030300010202     C                   exsr      invio
030400990920     C                   endif
030500990920     C*
030600910830     C                   ENDSR
030700000613     C***
030800010305
030900010305     C*----------------------------------------------------*
031000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031100010305     C*----------------------------------------------------*
031200020305     C     WRIVAB        BEGSR
031300070730     C*
031400071003     C* Considerazioni finali
031500071003     C                   if        VABRMA = *blanks
031600071003     C                   movel(P)  VABRMN        VABRMA
031700071003     C                   endif
031800100331     C*
031900100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
032000100331     C                   EVAL      VABCMR = %char(datcor)+'-'+%char(oracor)
032100100331     C                   EVAL      VABDCM = datcor
032200100331     C                   EVAL      VABDTS = datcor
032300100331     C                   EVAL      VABHMS = oracor
032400100331     C                   EVAL      VABCNT = 1
032500071003     C*
032600120411     C                   if        *in51 = *off and *in31 = *off
032700100331     C                   write     edivab00                                     => scarico il VAB
032800120411     C                   endif
032900010305     C*
033000010305     C                   ENDSR
033100990920
033200000801     C*----------------------------------------------------*
033300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033400000801     C*----------------------------------------------------*
033500010201     C     INZVAR        BEGSR
033600000801     C*
033700010201     C                   Z-ADD     *zeros        Num5_0
033800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033900020725     C                   MOVEL     '0'           FlgCAS            1
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034400000801     C*----------------------------------------------------*
034500020904     C     DEFCAM        BEGSR
034600080923     C*
034700120411     C                   SETOFF                                       505160
034800000801     C*
034900100331     C                   CLEAR                   EDIVAB00
035000100331     C                   CLEAR                   EDIVAT00
035100070730     C* Imposto i valori di default...
035200120629     C                   EVAL      VABCCM = 0661788
035300120629     C                   EVAL      VATCCM = 0661788
035400120629     C                   EVAL      VABLNP = 066
035500120629     C                   EVAL      VATLNP = 066
035600120629     C                   EVAL      VABCTR = 300
035700120629     C                   EVAL      VABCBO = '1'
035800120629     C                   EVAL      VABTSP = 'C'
035900070222     C* ... e poi verifico se sono stati passati come parametri
036000070222     C                   IF        vlrppt > *blanks
036100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036300070222     C                   EXSR      CHKNUM
036400070222     C                   IF        PiInt=*on
036500070222     C                   Z-ADD     PiVal         VABCCM
036600070222     C                   Z-ADD     PiVal         VATCCM
036700070222     C                   ENDIF
036800070222     C                   ENDIF
036900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037100070222     C                   EXSR      CHKNUM
037200070222     C                   IF        PiInt=*on
037300070222     C                   Z-ADD     PiVal         VABLNP
037400070222     C                   Z-ADD     PiVal         VATLNP
037500070222     C                   ENDIF
037600070222     C                   ENDIF
037700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037900070222     C                   EXSR      CHKNUM
038000070222     C                   IF        PiInt=*on
038100070222     C                   Z-ADD     PiVal         VABCTR
038200070222     C                   ENDIF
038300070928     C                   ENDIF
038400071009     C                   MOVEL     *blanks       wDISK             1
038500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038600071009     C                   IF        wDISK <> *blanks
038700070928     C                   SETON                                        50
038800070222     C                   ENDIF
038900120411     C                   IF        wDISK  = 'T'
039000120411     C                   SETOFF                                       50
039100120411     C                   SETON                                        51
039200120411     C                   ENDIF
039300120411     C                   MOVEL     *blanks       wDUPREC           1
039400120411     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
039500120411     C                   IF        %subst(vlrppt:16:2) <> *blanks
039600120411     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
039700120411     C                   ENDIF
039800120629     C*
039900120629     C* valido per nr.colli/peso/volume
040000120629     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
040100120629     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
040200120629     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
040300120629     C                   MOVEL     *blanks       wAdd              1            1/A/*blanks
040400120629     C                   EVAL      wAdd = %subst(vlrppt:18:1)
040500120411     C*
040600070222     C                   ENDIF
040700071009     C*
040800071009     C   50              EVAL      VABCTM = '7Q'
040900000801     C*
041000000801     C                   ENDSR
041100091223     C*----------------------------------------------------*
041200091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041300091223     C*----------------------------------------------------*
041400091223     C     REPNSP        BEGSR
041500091223     C*
041600091223     C                   EXSR      INZVAR
041700091223     C                   EXSR      DEFCAM
041800120411     C*
041900120411     C                   SETON                                        60
042000091223     C*
042100091223     C* NSP => Stacco un numeratore da AZNUM
042200091223     C                   clear                   TRUL33DS
042300091223     C                   eval      I33OPE = *zeros
042400091223     C                   eval      I33CNU = 302
042500091223     C                   eval      I33NUM = 1
042600091223     C                   movel     TRUL33DS      KPJBU
042700091223     C                   call      'TRUL33R'
042800091223     C                   parm                    KPJBA
042900091223     C                   movel     KPJBU         TRUL33DS
043000091223     C                   if        O33ERR = *zeros
043100091223     C                   z-add     O33NRF        VABNSP
043200091223     C                   z-add     O33NRF        VATNSP
043300091223     C                   else
043400091223     C                   SETON                                        31
043500091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043600091223     C                             + ' ' + 'VABNSP VATNSP'
043700091223     C                   endif
043800091223     C*
043900091223     C                   ENDSR
044000000801     C*----------------------------------------------------*
044100100331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
044200000801     C*----------------------------------------------------*
044300010201     C     IMPVAB        BEGSR
044400070530     C*
044500070530     C                   SETOFF                                       3132
044600070928     C*
044700070928     C                   MOVE(P)   vlrpoi        VABFGS
044800070928     C                   MOVE(P)   vlrpoi        VATFGS
044900070928     C*
045000070928     C                   MOVEL     datcor        VABAAS
045100070928     C                   MOVEL     datcor        VATAAS
045200070928     C                   MOVE      datcor        VABMGS
045300120629     C*
045400120629     C                   EVAL      VABRSD=%trim(%subst(vindta:12:35))
045500120629     C                   EVAL      VABRD2=%trim(%subst(vindta:12+35:17))
045600070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045700070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045800120629     C* ==
045900120629     C                   EVAL      VABIND=%trim(%subst(vindta:64:40))
046000120629     C                   EVAL      VABLOD=%trim(%subst(vindta:134:30))
046100120629     C                   EVAL      VABPRD=%trim(%subst(vindta:164:2))
046200120629     C                   EVAL      VABRMA=%trim(%subst(vindta:190:10))
046300120629     C                   EVAL      VABVCA=%trim(%subst(vindta:200:3))
046400120411     C*
046500120411     C* Reperimento campi NUMERICI
046600120411     C*
046700120629     C* CAD
046800120629     C                   EVAL      PiStr=%trim(%subst(vindta:124:10))
046900120629     C                   EXSR      CHKNUM
047000120629     C                   IF        PiInt=*on
047100120629     C                   Z-ADD     PiVal         Num5_0
047200120629     C                   MOVEL     Num5_0        VABCAD
047300120629     C                   ELSE
047400120629     C                   SETON                                        32
047500120629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600120629     C                             + ' ' + 'VABCAD'
047700120629     C                   ENDIF
047800120703     C*
047900120703     C* RMN
048000120703     C                   EVAL      PiStr=%trim(%subst(vindta:190+2:10-2))
048100120703     C                   EXSR      CHKNUM
048200120703     C                   IF        PiInt=*on
048300120703     C                   Z-ADD     PiVal         VABRMN
048400120703     C                   ELSE
048500120703     C                   SETON                                        32
048600120703     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700120703     C                             + ' ' + 'VABRMN'
048800120703     C                   ENDIF
048900120411     C* NSP
049000120629     C* parto dal secondo byte perché il primo è alfabetico
049100120411     C                   IF        *IN51 = *off
049200120629     C                   EVAL      PiStr=%subst(vindta:241+1:10-1)
049300120411     C                   EXSR      CHKNUM
049400120411     C                   IF        PiInt=*on
049500120411     C  N60              Z-ADD     PiVal         VABNSP
049600120411     C  N60              Z-ADD     PiVal         VATNSP
049700120411     C                   ELSE
049800120411     C                   SETON                                        32
049900120629     C                   Z-ADD     1             VABRMN
050000120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050100120703     C                             + ' ' + 'VABNSP VATNSP'
050200120411     C                   ENDIF
050300120411     C*
050400120411     C                   ELSE
050500120411     C*
050600120629     C                   EVAL      PiStr=%subst(vindta:241+1:10-1)
050700120411     C                   EXSR      CHKNUM
050800120411     C                   IF        PiInt=*on
050900120411     C  N60              Z-ADD     PiVal         VATNSP
051000120411     C                   ELSE
051100120411     C                   SETON                                        32
051200120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300120411     C                             + ' ' + 'VATNSP'
051400120411     C                   ENDIF
051500120411     C                   ENDIF
051600120411     C* NCL
051700120703     C* VABNCL è fisso a 1 per riga per cui non uso il dato reperito dal file
051800120703     C* Il dato lo reperisco lo stesso perché mi serve per calcolare il peso
051900120703     C                   EVAL      PiStr=%trim(%subst(vindta:166:5))
052000120703     C                   EXSR      CHKNUM
052100120703     C                   IF        PiInt=*on
052200120703     C                   EVAL      wDivPKB = PiVal
052300120703     C                   EVAL      PiVal = 1
052400120629     C                   SELECT
052500120629     C                   WHEN      wAdd = *blanks
052600120629     C                   Z-ADD     PiVal         VABNCL
052700120629     C                   WHEN      wAdd = 'A'
052800120629     C                   ADD       PiVal         VABNCL
052900120629     C                   WHEN      wAdd = 'C'
053000120629     C                   ADD       PiVal         VABNCL
053100120629     C                   ENDSL
053200120411     C                   ELSE
053300120411     C  N51              SETON                                        32
053400120411     C  N51              Z-ADD     *zeros        VABNCL
053500120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053600120411     C                             + ' ' + 'VABNCL'
053700120411     C                   ENDIF
053800120411     C* PKB
053900120629     C                   IF        %subst(vindta:171:10) <> *blanks
054000120629     C                   EVAL      PiStr=%trim(%subst(vindta:171:10))
054100120411     C                   EXSR      CHKNUM
054200120411     C                   IF        PiNum=*on
054300120629     C* il peso ci arriva KG
054400120629     C                   SELECT
054500120629     C                   WHEN      wAdd = *blanks
054600120703     C***                Z-ADD(H)  PiVal         VABPKB
054700120703     C                   EVAL(H)   VABPKB = PiVal/wDivPKB
054800120629     C                   WHEN      wAdd = 'A'
054900120703     C***                ADD       PiVal         VABPKB
055000120703     C                   EVAL      VABPKB = VABPKB + PiVal/wDivPKB
055100120629     C                   WHEN      wAdd = 'C'
055200120703     C***                Z-ADD(H)  PiVal         VABPKB
055300120703     C                   EVAL(H)   VABPKB = PiVal/wDivPKB
055400120629     C                   ENDSL
055500120411     C                   ELSE
055600120411     C  N51              SETON                                        32
055700120411     C  N51              Z-ADD     *zeros        VABPKB
055800120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055900120411     C                             + ' ' + 'VABPKB'
056000120411     C                   ENDIF
056100120629     C                   ENDIF
056200120629     C* VLB
056300120629     C                   IF        %subst(vindta:181:9) <> *blanks
056400120629     C                   EVAL      PiStr=%trim(%subst(vindta:181:9))
056500120629     C                   EXSR      CHKNUM
056600120629     C                   IF        PiNum=*on
056700120629     C                   SELECT
056800120629     C                   WHEN      wAdd = *blanks
056900120629     C                   Z-ADD(H)  PiVal         VABVLB
057000120629     C                   WHEN      wAdd = 'A'
057100120629     C                   ADD       PiVal         VABVLB
057200120629     C                   WHEN      wAdd = 'C'
057300120629     C                   Z-ADD(H)  PiVal         VABVLB
057400120629     C                   ENDSL
057500120629     C                   ELSE
057600120629     C                   SETON                                        32
057700120629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057800120629     C                             + ' ' + 'VABVLB'
057900120629     C                   ENDIF
058000120629     C                   ENDIF
058100120629     C* CAS
058200120629     C                   IF        %trim(%subst(vindta:203:12))<>*blanks
058300120629     C                             AND
058400120629     C                             %trim(%subst(vindta:203:12))<>*zeros
058500120629     C                             AND
058600120629     C                             %trim(%subst(vindta:203:12))<>'0,00'
058700120629     C                             AND
058800120629     C                             %trim(%subst(vindta:203:12))<>'000000000,00'
058900120629     C                             AND
059000120629     C                             %trim(%subst(vindta:203:12))<>'000000000.00'
059100120629     C                   MOVEL     '1'           FlgCAS
059200120629     C                   EVAL      PiStr=%trim(%subst(vindta:203:12))
059300120629     C                   EXSR      CHKNUM
059400120629     C                   IF        PiNum=*on
059500120629     C                   Z-ADD     PiVal         VABCAS
059600120629     C                   ELSE
059700120629     C                   SETON                                        32
059800120629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059900120629     C                             + ' ' + 'VABCAS'
060000120629     C                   ENDIF
060100120629     C                   ENDIF
060200100310     C*
060300100310     C* Se provincia nn valorizzata la reperisco
060400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
060500120412     C                   IF        VABNZD  = *blanks AND
060600120412     C                             VABPRD  = *blanks AND
060700120412     C                             VABCAD <> *blanks
060800100310     C                   CLEAR                   TISI95DS
060900100310     C                   EVAL      I95TCN = '3'
061000100310     C                   Z-ADD     datcor        I95DAT
061100100310     C                   EVAL      I95NAR = VABNZD
061200100310     C                   EVAL      I95CAP = VABCAD
061300100310     C                   EVAL      I95LOC = VABLOD
061400100310     C                   CALL      'TISI95R'
061500100310     C                   PARM                    TISI95DS
061600100310     C                   EVAL      VABPRD = O95PRV
061700100310     C                   ENDIF
061800070730     C*
061900070730     C* Considerazioni finali su CBO/CAS
062000070730     C                   IF        FlgCAS = '1'
062100070730     C                   IF        VABCBO = '1'
062200070730     C                   EVAL      VABCBO = '4'
062300070730     C                   ENDIF
062400070730     C                   IF        VABCBO = '2'
062500070730     C                   EVAL      VABCBO = '6'
062600070730     C                   ENDIF
062700070730     C                   ENDIF
062800010202     C*
062900000801     C* Ebbene...
063000000801     C                   ADD       1             §CTRMO
063100070530     C                   IF        *in31 <> *zeros OR
063200070530     C                             *in32 <> *zeros
063300000801     C                   ADD       1             §CTRNO
063400020725     C                   EVAL      x_vinflg = '2'
063500000801     C                   ELSE
063600010201     C                   ADD       1             §CTROKVB
063700000801     C                   ENDIF
063800000801     C*
063900000801     C                   ENDSR
064000100127     C*----------------------------------------------------*
064100100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
064200100127     C*----------------------------------------------------*
064300100127     C     WRIVAT_A      BEGSR
064400100127     C*
064500100331     C* Valorizzo l buffer di scrittura del EDIVAT
064600100127     C* - TIPO RECORD "A"
064700100331     C***                eval      VATTRC = 'A'
064800100331     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
064900100331     C***                exsr      wrivat
065000100127     C*
065100100127     C                   ENDSR
065200010201     C*----------------------------------------------------*
065300100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
065400010201     C*----------------------------------------------------*
065500071003     C     WRIVAT_B      BEGSR
065600010201     C*
065700100331     C* Valorizzo l buffer di scrittura del EDIVAT
065800070928     C* - TIPO RECORD "B"
065900120420     C                   eval      VATTRC = 'B'
066000120629     C                   eval      VATNOT = %trim(%subst(vindta:104:20))
066100120420     C                   exsr      wrivat
066200010201     C*
066300010201     C                   ENDSR
066400120411     C*----------------------------------------------------*
066500120411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
066600120411     C*----------------------------------------------------*
066700120411     C     WRIVAT_E      BEGSR
066800120411     C*
066900120411     C* Valorizzo l buffer di scrittura del FIVAT
067000120411     C* - TIPO RECORD "E"
067100120411     C*
067200120411     C                   eval      VATTRC = 'E'
067300120629     C                   eval      VATNOT = %trim(%subst(vindta:215:26))
067400120420     C                   exsr      wrivat
067500120420     C***
067600120420     C**Sviluppati i "CHI SONO" in relazione al numero colli
067700120420     C***
067800120420     C**Inizializzo progressivo collo su totale colli della spedizione
067900120420     C***                Z-ADD     1             wNumColloDA       2 0
068000120420     C***                MOVEL     *blanks       wNumColloCor      2
068100120420     C***                MOVEL     *blanks       wWhoIs            6
068200120420     C***                eval      wWhoIs = %trim(%subst(vindta:5:6))
068300120420     C***
068400120420     C***                DOW       wNumColloDA <= VABNCL
068500120420     C***                MOVEL(P)  wNumColloDA   wNumColloCor
068600120420     C***                EVAL      VATNOT =
068700120420     C***                             %subst(%char(%dec(%date() : *ISO)):3:2) +
068800120420     C***                             wNumColloCor + wWhoIs
068900120420     C***                exsr      wrivat
069000120420     C***                add       1             §CTROKVT
069100120420     C***
069200120420     C***                EVAL      wNumColloDA = wNumColloDA + 1
069300120420     C***                ENDDO
069400120411     C*
069500120411     C                   ENDSR
069600120420     C*----------------------------------------------------*
069700120420     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I"
069800120420     C*----------------------------------------------------*
069900120420     C     WRIVAT_I      BEGSR
070000120420     C*
070100120420     C* Valorizzo l buffer di scrittura del EDIVAT
070200120420     C* - TIPO RECORD "I"
070300120629     C***                if        %trim(%subst(vindta:293:35)) <> *blank
070400120629     C***                eval      VATTRC = 'I'
070500120629     C***                eval      VATNOT = %trim(%subst(vindta:293:35))
070600120629     C***                exsr      wrivat
070700120629     C***                endif
070800120420     C*
070900120420     C                   ENDSR
071000120420     C*----------------------------------------------------*
071100120420     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "J"
071200120420     C*----------------------------------------------------*
071300120420     C     WRIVAT_J      BEGSR
071400120420     C*
071500120420     C* Valorizzo l buffer di scrittura del EDIVAT
071600120420     C* - TIPO RECORD "J"
071700120629     C***                if        %trim(%subst(vindta:318:35)) <> *blank
071800120629     C***                eval      VATTRC = 'J'
071900120629     C***                eval      VATNOT = %trim(%subst(vindta:318:35))
072000120629     C***                exsr      wrivat
072100120629     C***                endif
072200120420     C*
072300120420     C                   ENDSR
072400100127     C*----------------------------------------------------*
072500100331     C*  SCARICO BUFFER EDIVAT
072600100127     C*----------------------------------------------------*
072700100127     C     WRIVAT        BEGSR
072800100127     C*
072900100127     C                   if        vatNOT <> *blanks
073000100331     C*
073100100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
073200100331     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
073300100331     C                   EVAL      VATCNT = 1
073400100331     C*
073500100127     C                   ADD       1             §CTROKVT
073600100331     C                   write     EDIVAT00
073700100127     C                   endif
073800100127     C*
073900100127     C                   ENDSR
074000020904
074100020904
074200010202     C*----------------------------------------------------*
074300100331     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
074400010202     C*----------------------------------------------------*
074500120412     C     PREVASX       BEGSR
074600010202     C*
074700100331     C* Compongo il nome del membro da dare al EDIVATWR
074800010202     C                   eval      parmbr = vlrhdl
074900010202     C                   movel     'M'           parmbr
075000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
075100010202     C                   eval      paropz = '1'
075200010202     C* Effettuo la chiamata al CLLE preposto
075300120411     C  N51              call(e)   'TITVEVTC'
075400010202     C                   parm                    parccm
075500010202     C                   parm                    parmbr
075600010202     C                   parm                    paropz
075700120411     C   51              call(e)   'TITVEVBC'
075800120411     C                   parm                    parccm
075900120411     C                   parm                    parmbr
076000120411     C                   parm                    paropz
076100120411     C*
076200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076300010202     C                   if        %error
076400010202     C                   movel     '1'           chkcall
076500010202     C                   else
076600010202     C                   movel     '0'           chkcall
076700010202     C                   endif
076800010202     C*
076900010202     C                   ENDSR
077000000801     C*----------------------------------------------------*
077100000801     C*  CONTROLLO NUMERICITA' CAMPI
077200000801     C*----------------------------------------------------*
077300000801     C     CHKNUM        BEGSR
077400081113     C*
077500081113     C                   IF        PiDecChr = *blanks
077600120629     C                   EVAL      PiDecChr = ','
077700081113     C                   ENDIF
077800091223     C*
077900081113     C                   callp(e)  UBISNUM_Check(PiStr
078000081113     C                                          :PiDecChr
078100081113     C                                          :PiVal
078200081113     C                                          :PiNum
078300081113     C                                          :PiInt)
078400081113     C*
078500000801     C                   IF        %error
078600000801     C                   EVAL      PiInt=*off
078700000801     C                   ENDIF
078800000801     C*
078900000801     C                   ENDSR
079000000801     C***
079100000801
079200990920      /TITLE Invio dei dati al punto operativo.
079300010202     C     invio         BEGSR
079400990920     C*
079500120412     C* 1° invio EDIVAT
079600010201     C                   reset                   dscmz
079700120412     C                   move      vatfgs        cmzdst
079800100331     C                   eval      cmzfld = 'EDIVATWR'
079900010201     C                   eval      cmzmbd = vlrhdl
080000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080100021009     C***                if        prmfir = *blanks
080200100331     C                   eval      cmzfla = 'EDIVAT0F'
080300100331     C                   eval      cmzmba = 'EDIVAT0F'
080400021009     C***                else
080500021009     C***                eval      cmzfla = prmfir
080600021009     C***                eval      cmzmba = prmfir
080700021009     C***                endif
080800010201     C                   eval      cmznrr = *zeros
080900020305     C                   move      §ctrokvt      cmznrr
081000021018     C                   eval      cmzlba = vlrfl1
081100010201     C                   call(e)   'TIS711C'
081200010201     C                   parm                    dscmz
081300010201     C                   parm      *blanks       esito
081400010205     C                   if        %error
081500010205     C                             or cmzerr = '1'
081600010205     C                             or esito  = '1'
081700010205     C                   eval      wrkesito = '3'
081800010205     C                   else
081900010201     C*
082000100331     C* 2° invio EDIVAB
082100120412     C                   if        *in51 = *off
082200010201     C                   reset                   dscmz
082300120412     C                   move      vabfgs        cmzdst
082400010201     C                   eval      cmzfld = vlrfou
082500010201     C                   eval      cmzmbd = vlrhdl
082600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082700021009     C***                if        prmfir = *blanks
082800100331     C                   eval      cmzfla = 'EDIVAB0F'
082900100331     C                   eval      cmzmba = 'EDIVAB0F'
083000021009     C***                else
083100021009     C***                eval      cmzfla = prmfir
083200021009     C***                eval      cmzmba = prmfir
083300021009     C***                endif
083400010201     C                   eval      cmznrr = *zeros
083500010201     C                   move      §ctrokvb      cmznrr
083600021018     C                   eval      cmzlba = vlrfl1
083700010201     C                   call(e)   'TIS711C'
083800010201     C                   parm                    dscmz
083900010201     C                   parm      *blanks       esito
084000010201     C                   if        %error
084100010201     C                             or cmzerr = '1'
084200010201     C                             or esito  = '1'
084300010201     C                   eval      wrkesito = '3'
084400010201     C                   endif
084500010205     C                   endif
084600120412     C                   endif
084700990920     C*
084800000613     C                   ENDSR
084900000613     C***
085000070411
085100070411     C     *pssr         BEGSR
085200070411     C*
085300070411     C                   if        %open(tivin00r)
085400070411     C                   close     tivin00r
085500070411     C                   endif
085600100331     C                   if        %open(edivabwr)
085700100331     C                   close     edivabwr
085800070411     C                   endif
085900100331     C                   if        %open(edivatwr)
086000100331     C                   close     edivatwr
086100070411     C                   endif
086200070411     C*
086300070411     C* Effettuo la chiamata al CLLE preposto
086400120411     C  N51              call(e)   'TITVEVTC'
086500120411     C                   parm                    parccm
086600120411     C                   parm                    parmbr
086700120411     C                   parm      '2'           paropz
086800120411     C   51              call(e)   'TITVEVBC'
086900120411     C                   parm                    parccm
087000120411     C                   parm                    parmbr
087100120411     C                   parm      '2'           paropz
087200070411     C*
087300070411     C                   eval      wrkesito = '2'
087400070411     C*
087500070411     C                   seton                                        LR
087600070411     C*
087700070411     C                   ENDSR     '*CANCL'
087800070411     C***
087900070411
088000990910
088100000613     C     *inzsr        BEGSR
088200990910     C*
088300990910     C     *entry        plist
088400990920     C                   parm                    tivlrds
088500990921     C                   parm      wrkesito      esito
088600000724     C                   parm                    prmlit
088700000710     C                   parm                    prmfir
088800000613     C*
088900000830     C* CALCOLA LA DATA CORRENTE
089000100331     C                   time                    wn14             14 0
089100100331     C                   movel     wn14          oracor            6 0          *ORA
089200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
089300091223     C                   eval      datcor = %dec(%date() : *ISO)
089400000830     C*
089500000613     C                   ENDSR
089600000613     C***
