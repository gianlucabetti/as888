000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800051012     Ftitvo7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100051012     Ftitvo7ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500050329     D depspe          s              9    INZ(*blanks)
003600050329     D curspe          s              9    INZ(*blanks)
003700050329     D cntspe          s              9  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400030107     D  Num7_0         s              7  0
004500051012
004600051012     d alfa7           s              7
004700051116     d alfa15          s             15    varying inz(*blanks)
004800051116     d wbarcode        s             15
004900000830
005000000830     D*------------------
005100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200000830     D*------------------
005300000830     D WLBDA8          DS                  INZ
005400000830     D  G08DAT                 1      8  0
005500000830     D  G08INV                 9     16  0
005600000830     D  G08ERR                17     17
005700000830     D  G08TGI                18     22  0
005800000830     D*
005900051013     D*------------------
006000051013     D* DS REPERIMENTO NUMERATORE
006100051013     D*------------------
006200051013     D trul33ds      e ds                  inz
006300051013     D*------------------
006400051013     D* DS ARCHITETTURA
006500051013     D*------------------
006600051013     D kpjba         e ds                  inz
006700051013     D*------------------
006800990908
006900010201
007000010201
007100990915     C                   time                    wrkdata
007200990915     C                   time                    wrkora
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600000724     C*
007700000724     C* SE OCCORRE SPEDIRE IN FILIALE
007800021017     C                   if        vlrpoi <> *zeros
007900000724     C*
008000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200021017     C     vlrpoi        chain     azorg01l
008300000724     C                   if        %found
008400000616     C                   movel(p)  CMD(1)        depcmd
008500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000724     C*
009300000724     C                   endif
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000616     C*
010600051012     C                   if        not %open(titvo7ps)
010700051012     C                   open      titvo7ps
010800000616     C                   except    testdett
010900000613     C                   endif
011000000613     C*
011100030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300000613     C*
011400010202     C* Effettuo la chiamata al CLLE preposto
011500031201     C                   call(e)   'TITVVTC'
011600010202     C                   parm                    parccm
011700010202     C                   parm                    parmbr
011800010202     C                   parm      '2'           paropz
011900010202     C*
012000051012     C                   if        %open(titvo7ps)
012100000616     C                   except    findett
012200051012     C                   close     titvo7ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000000616     C*
013100010201     C                   seton                                        LR
013200000613
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900051012     C                   if        not %open(titvo7p)
014000051012     C                   open      titvo7p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500051012     C                   if        %open(titvo7p)
014600051012     C                   close     titvo7p
014700990914     C                   endif
014800990910     C*
014900000613     C                   ENDSR
015000000613     C***
015100990908
015200000801
015300910830     C*--------------------------------------------------------
015400030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
015500910830     C*--------------------------------------------------------
015600030226     C     RWFILE        BEGSR
015700990910     C*
015800990914     C                   if        not %open(tivin00r)
015900990908     C                   open      tivin00r
016000990914     C                   endif
016100021017     C                   if        not %open(fivabwwr)
016200021017     C                   open      fivabwwr
016300990914     C                   endif
016400021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500020305     C                   exsr      prevat
016600010201     C*
016700010202     C                   if        chkcall = '0'
016800010202     C*
016900021017     C                   if        not %open(fivatwwr)
017000021017     C                   open      fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   clear                   §CTROKVB          5 0
017400020305     C                   clear                   §CTROKVT          5 0
017500000801     C                   clear                   §CTRMO            5 0
017600000801     C                   clear                   §CTRNO            5 0
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100000905     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000801     C*
018400000801     C                   if        *in70 = *off
018500000801     C                             and
018600000801     C                             (vinflg = *blanks
018700000801     C                              or vinflg = '0'
018800000801     C                              or vinflg = '2')
018900000801     C*
019000000801     C                   clear                   vinmsg
019100030310     C                   eval      vinflg = '1'
019200030310     C*
019300030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019400051012     C                   eval      PiStr=%trim(%subst(vindta:187:8))
019500030411     C                   movel(p)  PiStr         curspe
019600030411     C                   add       1             cntspe
019700030411     C*
019800030411     C                   if        cntspe = 1
019900030411     C* A rottura inizializzo campi di totalizzazione
020000030411     C                   clear                   fivab000
020100030411     C*
020200030411     C                   exsr      impvab                                       => carico VAB
020300031201     C                   exsr      exevat                                       => carico VAT
020400030411     C                   eval      depspe = curspe
020500030411     C                   else
020600030411     C                   if        curspe <> depspe
020700030411     C                   exsr      wrivab                                       => scarico VAB
020800030411     C* A rottura inizializzo campi di totalizzazione
020900030411     C                   clear                   fivab000
021000030411     C*
021100030411     C                   exsr      impvab                                       => carico VAB
021200031201     C                   exsr      exevat                                       => carico VAT
021300030411     C                   eval      depspe = curspe
021400030411     C                   else
021500030411     C                   exsr      impvab                                       => carico VAB
021600031201     C                   exsr      exevat                                       => carico VAT
021700030411     C                   endif
021800030411     C                   endif
021900000905     C*
022000000905     C                   else
022100000905     C                   eval      vinflg = '1'
022200000905     C                   endif
022300030411     C                   endif
022400000905     C*
022500000905     C  N70              update    tivin000
022600000905     C*
022700991022     C  N70              ENDdo
022800030430     C*
022900030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
023000030430     C                   exsr      wrivab                                       => scarico VAB
023100010202     C*
023200010202     C                   endif
023300990910
023400990910     C* Se non ci sono record con errori ...
023500000710     C                   if        §ctrno = 0
023600990910     C* ... restituisco esito OK.
023700990921     C                   eval      wrkesito = '0'
023800990910     C                   else
023900010201     C                   if        §ctrokvb > 0
024000990921     C                   eval      wrkesito = '1'
024100000710     C                   else
024200000710     C                   eval      wrkesito = '2'
024300990910     C                   endif
024400000710     C                   endif
024500990910     C*
024600990914     C                   if        %open(tivin00r)
024700990908     C                   close     tivin00r
024800990914     C                   endif
024900021017     C                   if        %open(fivabwwr)
025000021017     C                   close     fivabwwr
025100990914     C                   endif
025200021017     C                   if        %open(fivatwwr)
025300021017     C                   close     fivatwwr
025400010201     C                   endif
025500021017     C*
025600021017     C                   if        vlrpoi <> 999
025700021017     C                   eval      vlrpoi = vlrpoi
025800021017     C                   endif
025900990910     C*
026000010201     C                   if        §ctrokvb > 0
026100021017     C                             and vlrpoi > *zeros
026200010202     C                   exsr      invio
026300990920     C                   endif
026400990920     C*
026500910830     C                   ENDSR
026600000613     C***
026700990920
026800000801     C*----------------------------------------------------*
026900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027000000801     C*----------------------------------------------------*
027100010201     C     INZVAR        BEGSR
027200000801     C*
027300010201     C                   Z-ADD     *zeros        Num5_0
027400030107     C                   Z-ADD     *zeros        Num7_0
027500051012     c                   Clear                   alfa7
027600051116     c                   Clear                   alfa15
027700051116     c                   Clear                   wbarcode
027800030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027900030310     C                   MOVEL     '0'           FlgCAS            1
028000000801     C*
028100000801     C                   ENDSR
028200000801     C*----------------------------------------------------*
028300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028400000801     C*----------------------------------------------------*
028500000801     C     DEFCAM        BEGSR
028600000801     C*
028700020619     C* Imposto i valori di default...
028800051013     C                   Z-ADD     0893279       VABCCM
028900051013     C                   Z-ADD     0893279       VATCCM
029000021017     C                   Z-ADD     vlrpoi        VABFGS
029100021017     C                   Z-ADD     vlrpoi        VATFGS
029200051013     C                   Z-ADD     089           VABLNP
029300051013     C                   Z-ADD     089           VATLNP
029400051013     C                   Z-ADD     300           VABCTR
029500021017     C                   MOVEL     '7Q'          VABCTM
029600050317     C                   MOVEL     'C'           VABTSP
029700050321     C                   MOVEL     '1'           VABCBO
029800051013     c                   Eval      vabnas = 'MATERIALE ELETTRICO'
029900020619     C* ... e poi verifico se sono stati passati come parametri
030000020619     C                   IF        vlrppt > *blanks
030100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030200020619     C                   EXSR      CHKNUM
030300020619     C                   IF        PiInt=*on
030400020619     C                   Z-ADD     PiVal         VABCCM
030500020619     C                   Z-ADD     PiVal         VATCCM
030600020619     C                   ENDIF
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABLNP
031100020619     C                   Z-ADD     PiVal         VATLNP
031200020619     C                   ENDIF
031300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031400020619     C                   EXSR      CHKNUM
031500020619     C                   IF        PiInt=*on
031600020619     C                   Z-ADD     PiVal         VABCTR
031700020619     C                   ENDIF
031800020619     C                   ENDIF
031900000801     C*
032000000801     C                   ENDSR
032100000801     C*----------------------------------------------------*
032200030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032300000801     C*----------------------------------------------------*
032400010201     C     IMPVAB        BEGSR
032500000801     C*
032600020305     C                   EXSR      INZVAR
032700020305     C                   EXSR      DEFCAM
032800010305     C*
032900030226     C                   Z-ADD     *zeros        errore            1 0
033000000830     C                   MOVEL     datcor        VABAAS
033100000830     C                   MOVE      datcor        VABMGS
033200030226     C*
033300051012     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
033400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033600020117     C* ==
033700051012     C                   EVAL      VABRD2=%trim(%subst(vindta:17+35:40-35))
033800050329     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
033900050329     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034000050329     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
034100051012     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
034200051012     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
034300051012     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
034400050318
034500051012     C                   IF        %trim(%subst(vindta:16:1)) = '1'
034600050329     C                   EVAL      VABFFD = 'S'
034700050318     C                   ENDIF
034800050317
034900030226     C* CAP
035000050329     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
035100010201     C                   EXSR      CHKNUM
035200010201     C                   IF        PiInt=*on
035300010201     C                   Z-ADD     PiVal         Num5_0
035400021017     C                   MOVEL     Num5_0        VABCAD
035500010201     C                   ELSE
035600010201     C                   ADD       1             errore
035700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035800021017     C                             + ' ' + 'VABCAD'
035900010201     C                   ENDIF
036000051012     C* NSP/RMN
036100051012     c                   Eval      alfa7=%trim(%subst(vindta:187:2)) +
036200051012     c                                   %trim(%subst(vindta:190:5))
036300051012     C                   EVAL      PiStr=alfa7
036400010201     C                   EXSR      CHKNUM
036500051013    1C                   IF        PiInt=*on
036600031201     C                   Z-ADD     PiVal         VABRMN
036700051012      * Il numero della spedizione è indicato in un campo alfa (dalla 187 lungo 10)
036800051012      * al terzo byte ci deve essere una "/" che separa l'anno (AA) dal n. documento
036900051012      * del cliente (il cliente ha detto che il suo n. documento è lungo 5 io quindi prendo
037000051012      * per buono lungo 5)
037100051013      * se non trovo la barra al terzo byte stacco un numero spedizione dal numeratore
037200051013    2c                   If        %subst(vindta:189:1) = '/'
037300051012     C                   Z-ADD     PiVal         VABNSP
037400051013     C                   Z-ADD     PiVal         VATNSP
037500051013   x2c                   Else
037600051012      * numero bolla solo a cambio di RMN
037700051013    3c                   If        depspe <> curspe
037800051012     C                   clear                   TRUL33DS
037900051012     C                   eval      I33OPE = *zeros
038000051012     C                   eval      I33CNU = 302
038100051012     C                   eval      I33NUM = 1
038200051012     C                   movel     TRUL33DS      KPJBU
038300051012     C                   call      'TRUL33R'
038400051012     C                   parm                    KPJBA
038500051012     C                   movel     KPJBU         TRUL33DS
038600051013    4C                   if        O33ERR = *zeros
038700051012     C                   z-add     O33NRF        VABNSP
038800051013     C                   z-add     O33NRF        VATNSP
038900051013   x4C                   else
039000051012     C                   ADD       1             errore
039100051012     C                   EVAL      vinmsg = %trimr(vinmsg)
039200051013     C                             + ' ' + 'VABNSP VATNSP'
039300051013    4C                   endif
039400051013    3C                   endif
039500051013    2C                   ENDIF
039600051013   x1C                   ELSE
039700050329     C                   ADD       1             errore
039800050329     C                   EVAL      vinmsg = %trimr(vinmsg)
039900051013     C                             + ' ' + 'VABNSP VATNSP VABRMN'
040000051013    1C                   ENDIF
040100030226     C* NCL
040200050329     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
040300050317     C                   EXSR      CHKNUM
040400050317     C                   IF        PiInt=*on
040500050317     C                   Z-ADD     PiVal         VABNCL
040600050317     C                   ELSE
040700050317     C                   ADD       1             errore
040800050317     C                   EVAL      vinmsg = %trimr(vinmsg)
040900050317     C                             + ' ' + 'VABNCL'
041000050317     C                   ENDIF
041100030226     C* PKB
041200051013     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
041300030107     C                   EXSR      CHKNUM
041400030107     C                   IF        PiNum=*on
041500050317     C                   Z-ADD     PiVal         VABPKB
041600030107     C                   ELSE
041700030107     C                   ADD       1             errore
041800030107     C                   EVAL      vinmsg = %trimr(vinmsg)
041900030107     C                             + ' ' + 'VABPKB'
042000030107     C                   ENDIF
042100050317     C* CAS/VCA
042200050329     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
042300050317     C                   EXSR      CHKNUM
042400050317     C                   IF        PiNum=*on
042500050321     C                   Z-ADD     PiVal         VABCAS
042600051013     c                   If        vabcas > 0
042700050329     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
042800051013     C                   MOVEL     '1'           FlgCAS
042900051013     c                   EndIf
043000050317     C                   ELSE
043100050317     C                   SETON                                        32
043200050317     C                   Z-ADD     *zeros        VABCAS
043300050317     C                   EVAL      vinmsg = %trimr(vinmsg)
043400050317     C                             + ' ' + 'VABCAS'
043500050317     C                   ENDIF
043600050329     C* IAS/VAS
043700050329     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
043800050329     C                   EXSR      CHKNUM
043900050329     C                   IF        PiNum=*on
044000050329     C                   Z-ADD     PiVal         VABIAS
044100051013     c                   If        vabias > 0
044200050329     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
044300051013     c                   EndIf
044400050329     C                   ELSE
044500050329     C                   SETON                                        32
044600050329     C                   Z-ADD     *zeros        VABIAS
044700050329     C                   EVAL      vinmsg = %trimr(vinmsg)
044800050329     C                             + ' ' + 'VABIAS'
044900050329     C                   ENDIF
045000030310     C*
045100050321     C* Considerazioni finali su CBO/CAS
045200030310     C                   IF        FlgCAS = '1'
045300050321     C                   IF        VABCBO = '1'
045400010205     C                   EVAL      VABCBO = '4'
045500050321     C                   ENDIF
045600050321     C                   IF        VABCBO = '2'
045700050321     C                   EVAL      VABCBO = '6'
045800050321     C                   ENDIF
045900010205     C                   ENDIF
046000020305     C*
046100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046200011113     C                   EXSR      CHKIMPDIV
046300010202     C*
046400000801     C* Ebbene...
046500000801     C                   ADD       1             §CTRMO
046600010201     C                   IF        errore <> *zeros
046700000801     C                   ADD       1             §CTRNO
046800000801     C                   EVAL      recko = vindta
046900000801     C                   EXCEPT    rigadett
047000000801     C                   EVAL      vinflg = '2'
047100000801     C                   ELSE
047200010201     C                   ADD       1             §CTROKVB
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600030226     C*----------------------------------------------------*
047700030226     C*  SCARICAMENTO BUFFER RECORDS VAB
047800030226     C*----------------------------------------------------*
047900030226     C     WRIVAB        BEGSR
048000030310     C*
048100030310     C                   write     FIVAB000
048200030226     C*
048300030226     C                   ENDSR
048400030226     C*----------------------------------------------------*
048500030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
048600030226     C*----------------------------------------------------*
048700031201     C     EXEVAT        BEGSR
048800030226     C*
048900030226     C                   MOVEL     datcor        VATAAS
049000030226     C*
049100030415     C* Valorizzo il barcode
049200031201     C                   EVAL      VATTRC='E'
049300051116     c**!!!              EVAL      VATNOT=%trim(%subst(vindta:331:15))
049400051116     c                   Eval      alfa15=%trim(%subst(vindta:331:15))
049500051116     c                   Eval      wbarcode = *all'0'
049600051116     C                   Move      alfa15        wbarcode
049700051116     c                   Eval      vatnot = wbarcode
049800031201     C                   exsr      wrivat                                       => scarico VAT
049900050329     C*
050000050329     C* Valorizzo il telefono destinatario (se valorizzato)
050100050329     C                   IF        %subst(vindta:92:15) <> *blanks
050200050329     C                   EVAL      VATTRC='B'
050300050329     C                   EVAL      VATNOT=%trim(%subst(vindta:92:15))
050400050329     C                   exsr      wrivat                                       => scarico VAT
050500050329     C                   ENDIF
050600030226     C*
050700030226     C                   ENDSR
050800010201     C*----------------------------------------------------*
050900021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051000010201     C*----------------------------------------------------*
051100020305     C     WRIVAT        BEGSR
051200010201     C*
051300021017     C                   write     FIVAT000
051400010201     C*
051500010201     C                   ENDSR
051600010202     C*----------------------------------------------------*
051700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051800010202     C*----------------------------------------------------*
051900020305     C     PREVAT        BEGSR
052000010202     C*
052100021017     C* Compongo il nome del membro da dare al FIVATWWR
052200010202     C                   eval      parmbr = vlrhdl
052300010202     C                   movel     'M'           parmbr
052400050317     C                   eval      parccm = %subst(vlrKSC:2:7)
052500010202     C                   eval      paropz = '1'
052600010202     C* Effettuo la chiamata al CLLE preposto
052700031201     C                   call(e)   'TITVVTC'
052800010202     C                   parm                    parccm
052900010202     C                   parm                    parmbr
053000010202     C                   parm                    paropz
053100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053200010202     C                   if        %error
053300010202     C                   movel     '1'           chkcall
053400010202     C                   else
053500010202     C                   movel     '0'           chkcall
053600010202     C                   endif
053700010202     C*
053800010202     C                   ENDSR
053900000801     C*----------------------------------------------------*
054000000801     C*  CONTROLLO NUMERICITA' CAMPI
054100000801     C*----------------------------------------------------*
054200000801     C     CHKNUM        BEGSR
054300050329
054400050329     C                   IF        PiDecChr = *blanks
054500050329     C                   EVAL      PiDecChr = ','
054600050329     C                   ENDIF
054700000801     C*
054800000801     C                   call(e)   'ISNUMERIC'
054900000801     C                   PARM                    PiStr            30
055000050329     C                   PARM                    PiDecChr          1
055100000801     C                   PARM      *ZEROS        PiVal            30 9
055200000801     C                   PARM      '0'           PiInt             1
055300000801     C                   PARM      '0'           PiNum             1
055400000801     C                   IF        %error
055500000801     C                   EVAL      PiInt=*off
055600030107     C                   EVAL      PiNum=*off
055700000801     C                   ENDIF
055800000801     C*
055900000801     C                   ENDSR
056000000801     C***
056100000801
056200011113
056300011113     C*----------------------------------------------------*
056400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056500011113     C*----------------------------------------------------*
056600011113     C     CHKIMPDIV     BEGSR
056700011113     C*
056800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056900011113     C                   Z-ADD     *zeros        wrkDec            9 9
057000011113     C*
057100011113     C* Come prima cosa effettuo considerazioni sulla divisa
057200011113     C                   IF        vabIAS > *zeros
057300011113     C                   IF        vabVAS <> 'EUR'
057400011113     C                   EVAL      vabVAS =  'ITL'
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   IF        vabCAS > *zeros
057900011113     C                   IF        vabVCA <> 'EUR'
058000011113     C                   EVAL      vabVCA =  'ITL'
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C                   IF        vabVMD > *zeros
058500020305     C                   IF        vabVAD <> 'EUR'
058600011113     C                   EVAL      vabVAD =  'ITL'
058700011113     C                   ENDIF
058800011113     C                   ENDIF
058900011113     C*
059000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059100011113     C                   Z-ADD     vabIAS        wrkDec
059200011113     C                   IF        wrkDec > *zeros
059300011113     C                   IF        vabVAS = 'ITL'
059400011113     C                   EVAL      vabIAS = *zeros
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059900011113     C                   Z-ADD     vabCAS        wrkDec
060000011113     C                   IF        wrkDec > *zeros
060100011113     C                   IF        vabVCA = 'ITL'
060200011113     C                   EVAL      vabCAS = *zeros
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060700011113     C                   Z-ADD     vabVMD        wrkDec
060800011113     C                   IF        wrkDec > *zeros
060900011113     C                   IF        vabVAD = 'ITL'
061000011113     C                   EVAL      vabVMD = *zeros
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C                   ENDSR
061500011113     C***
061600021017
061700000801
061800000801
061900990920      /TITLE Invio dei dati al punto operativo.
062000010202     C     invio         BEGSR
062100990920     C*
062200021017     C* 1° invio FIVAT
062300010201     C                   reset                   dscmz
062400021017     C                   move      vlrpoi        cmzdst
062500021017     C                   eval      cmzfld = 'FIVATWWR'
062600010201     C                   eval      cmzmbd = vlrhdl
062700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062800021017     C                   eval      cmzfla = 'FIVAT00F'
062900021017     C                   eval      cmzmba = 'FIVAT00F'
063000010201     C                   eval      cmznrr = *zeros
063100020305     C                   move      §ctrokvt      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010205     C                   if        %error
063700010205     C                             or cmzerr = '1'
063800010205     C                             or esito  = '1'
063900010205     C                   eval      wrkesito = '3'
064000010205     C                   else
064100010201     C*
064200021017     C* 2° invio FIVAB
064300010201     C                   reset                   dscmz
064400021017     C                   move      vlrpoi        cmzdst
064500010201     C                   eval      cmzfld = vlrfou
064600010201     C                   eval      cmzmbd = vlrhdl
064700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064800021017     C                   eval      cmzfla = 'FIVAB00F'
064900021017     C                   eval      cmzmba = 'FIVAB00F'
065000010201     C                   eval      cmznrr = *zeros
065100010201     C                   move      §ctrokvb      cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300010201     C                   call(e)   'TIS711C'
065400010201     C                   parm                    dscmz
065500010201     C                   parm      *blanks       esito
065600010201     C                   if        %error
065700010201     C                             or cmzerr = '1'
065800010201     C                             or esito  = '1'
065900010201     C                   eval      wrkesito = '3'
066000010201     C                   endif
066100010205     C                   endif
066200990920     C*
066300000613     C                   ENDSR
066400000613     C***
066500070411
066600070411     C     *pssr         BEGSR
066700070411     C*
066800070411     C                   if        %open(tivin00r)
066900070411     C                   close     tivin00r
067000070411     C                   endif
067100070411     C                   if        %open(fivabwwr)
067200070411     C                   close     fivabwwr
067300070411     C                   endif
067400070411     C                   if        %open(fivatwwr)
067500070411     C                   close     fivatwwr
067600070411     C                   endif
067700070411     C*
067800070411     C* Effettuo la chiamata al CLLE preposto
067900070411     C                   call(e)   'TITVVTC'
068000070411     C                   parm                    parccm
068100070411     C                   parm                    parmbr
068200070411     C                   parm      '2'           paropz
068300070411     C*
068400070411     C                   eval      wrkesito = '2'
068500070411     C*
068600070411     C                   seton                                        LR
068700070411     C*
068800070411     C                   ENDSR     '*CANCL'
068900070411     C***
069000070411
069100990910
069200000613     C     *inzsr        BEGSR
069300990910     C*
069400990910     C     *entry        plist
069500990920     C                   parm                    tivlrds
069600990921     C                   parm      wrkesito      esito
069700000724     C                   parm                    prmlit
069800000710     C                   parm                    prmfir
069900000613     C*
070000000830     C* CALCOLA LA DATA CORRENTE
070100000830     C                   time                    wn14             14 0
070200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070300000830     C                   z-add     wn8           g08dat
070400000830     C                   z-add     *zeros        g08inv
070500000830     C                   movel     '0'           g08err
070600000830     C                   call      'XSRDA8'
070700000830     C                   parm                    wlbda8
070800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070900000830     C*
071000000613     C                   ENDSR
071100000613     C***
071200990908
071300051012     Otitvo7p   E            riepilogo         2
071400990915     O                                              'Upload via Internet'
071500990915     O                                           +1 'Traduzione TIVIN00R -'
071600021017     O                                           +1 'FIVABWWR/FIVATWWR'
071700010201     O                                           +1 'Report testate bolle'
071800990915     O          E            riepilogo   2
071900990915     O                       wrkdata
072000990915     O                       wrkora              +1
072100990915     O                       procname            +1
072200990915     O          E            riepilogo   2
072300990915     O                                              'Cliente..................:'
072400990915     O                       VABCCM        z     +1
072500990915     O          E            riepilogo   2
072600990920     O                                              'Riferimento Strategi.....:'
072700990920     O                       vlrhdl              +1
072800990915     O          E            riepilogo   2
072900990915     O                                              'Giusti...................:'
073000010201     O                       §CTROKVB      2   +  1
073100990915     O          E            riepilogo   2
073200990915     O                                              'Sbagliati e corretti.....:'
073300971022     O                       §CTRMO        2   +  1
073400990915     O          E            riepilogo   2
073500990915     O                                              'Sbagliati e scartati.....:'
073600971022     O                       §CTRNO        2   +  1
073700000613
073800051012     Otitvo7ps  E            testdett          2
073900000613     O                                              'Upload via Internet'
074000000613     O                                           +1 'Traduzione TIVIN00R -'
074100021017     O                                           +1 'FIVABWWR/FIVATWWR'
074200010201     O                                           +1 'Report testate bolle'
074300000616     O          E            testdett    3
074400000613     O                                           +2 'N° rec'
074500000613     O                                           +3 'Anteprima contenuto'
074600000616     O          E            rigadett    2
074700000613     O                       rrnum               +2
074800000621     O                       recko               +3
074900000616     O          E            findett     2
075000000613     O                       wrkdata
075100000613     O                       wrkora              +1
075200000613     O                       procname            +1
075300000616     O          E            findett     2
075400000613     O                                              'Cliente..................:'
075500000613     O                       VABCCM        z     +1
075600000616     O          E            findett     2
075700000613     O                                              'Riferimento Strategi.....:'
075800000613     O                       vlrhdl              +1
075900000616     O          E            findett     2
076000000613     O                                              'Giusti...................:'
076100010201     O                       §CTROKVB      2   +  1
076200000616     O          E            findett     2
076300000613     O                                              'Sbagliati e corretti.....:'
076400000613     O                       §CTRMO        2   +  1
076500000616     O          E            findett     2
076600000613     O                                              'Sbagliati e scartati.....:'
076700000613     O                       §CTRNO        2   +  1
076800000512** CMD - COMANDI CL
076900051012OVRPRTF FILE(TITVO7P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
077000051012OVRPRTF FILE(TITVO7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077100051012DLTOVR FILE(TITVO7P TITVO7PS) LVL(*)
077200000512
077300000512
