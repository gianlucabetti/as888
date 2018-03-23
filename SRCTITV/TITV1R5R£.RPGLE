000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090930     Ftitv1r5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090930     Ftitv1r5ps O    f  198        PRINTER usropn
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
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000090930     D curSped         s             15    INZ(*blanks)
004100090930     D depSped         s             15    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100090930
005200090930     D*------------------
005300090930     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005400090930     D*------------------
005500090930     D* 10+3
005600090930     D                 DS
005700090930     D  WI15                   1     10
005800090930     D  WD15                  11     13
005900090930     D  WASC15                 1     13
006000090930     D*  6+1
006100090930     D                 DS
006200090930     D  WI09                   1      6
006300090930     D  WD09                   7      7
006400090930     D  WASC09                 1      7
006500090930     D*  2+3
006600090930     D                 DS
006700090930     D  WI07                   1      2
006800090930     D  WD07                   3      5
006900090930     D  WASC07                 1      5
007000090930     D*
007100090114
007200090930     D*---------------------------------------------------------
007300090930     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
007400090930     D*---------------------------------------------------------
007500090930     D                 ds
007600090930     D vhbatb                  1      1
007700090930     D vhbccm                  2      8
007800090930     D vhblnp                  9     11
007900090930     D vhbaas                 12     15
008000090930     D vhbmgs                 16     19
008100090930     D vhbnrs                 20     21
008200090930     D vhbnsp                 22     28
008300090930     D vhbcbo                 29     30
008400090930     D vhblna                 31     33
008500090930     D vhbrsd                 34     68
008600090930     D vhbrd2                 69    103
008700090930     D vhbind                104    138
008800090930     D vhbcad                139    147
008900090930     D vhblod                148    182
009000090930     D vhbprd                183    184
009100090930     D vhbnzd                185    187
009200090930     D vhbgc1                188    189
009300090930     D vhbgc2                190    191
009400090930     D vhbctr                192    194
009500090930     D vhbtsp                195    195
009600090930     D vhbias                196    208
009700090930     D vhbvas                209    211
009800090930     D vhbnas                212    226
009900090930     D vhbncl                227    231
010000090930     D vhbpkb                232    238
010100090930     D vhbvlb                239    243
010200090930     D vhbqft                244    256
010300090930     D vhbcas                257    269
010400090930     D vhbtic                270    271
010500090930     D vhbvca                272    274
010600090930     D vhbgca                275    276
010700090930     D vhbrmn                277    291
010800090930     D vhbrma                292    306
010900090930     D vhbncd                307    313
011000090930     D vhbnca                314    320
011100090930     D vhbxco                321    321
011200090930     D vhbnot                322    356
011300090930     D vhbnt2                357    391
011400090930     D vhbznc                392    393
011500090930     D vhbctm                394    395
011600090930     D vhbffd                396    396
011700090930     D vhbdcr                397    404
011800090930     D vhbtcr                405    405
011900090930     D vhbhcr                406    409
012000090930     D vhbcts                410    411
012100090930     D vhbftm                412    412
012200090930     D vhbvmd                413    425
012300090930     D vhbvad                426    428
012400090930     D vhbgma                429    430
012500090930     D vhbgga                431    432
012600090930     D vhbgva                433    434
012700090930     D vhbtc1                435    435
012800090930     D vhbtc2                436    436
012900090930     D vhbscl                437    437
013000090930     D vhbant                438    446
013100090930     D vhbrmo                447    471
013200090930     D vhbcmo                472    480
013300090930     D vhbnmo                481    483
013400090930     D  X_rec                  1    483    inz
013500090930     D*
013600081217
013700081217     D*------------------
013800081217     D* LINKING A DEFINIZIONI ESTERNE
013900081217     D*------------------
014000081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
014100081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
014200081217
014300081217
014400010201
014500081217
014600090930     C*
014700090930     C* Inizializzo flag e contatori operativi
014800090930     C                   movel     '0'           flgGiro           1
014900090930     C                   movel     '0'           flgMulti          1
015000090930     C                   movel     '1'           flgStato          1
015100090930     C                   movel     '0'           flgOk             1
015200090930     C                   z-add     *zeros        cntNonEl         10 0
015300090930     C                   z-add     *zeros        depfil            3 0
015400090930     C                   z-add     *zeros        invfil            3 0
015500090930     C*
015600990915     C                   time                    wrkdata
015700990915     C                   time                    wrkora
015800000913     C                   reset                   rrnum
015900990921     C                   reset                   esito
016000990921     C                   reset                   wrkesito
016100000724     C*
016200000724     C* SE OCCORRE SPEDIRE IN FILIALE
016300000724     C                   if        vlrpoi <> *zeros
016400000724     C*
016500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
016600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016700000724     C     vlrpoi        chain     azorg01l
016800000724     C                   if        %found
016900000616     C                   movel(p)  CMD(1)        depcmd
017000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017100000616     C*
017200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017300011113     C                   Z-ADD     150           LENGH            15 5
017400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017500000616     C                   PARM                    depcmd
017600000616     C                   PARM                    LENGH
017700000724     C*
017800000724     C                   endif
017900000724     C                   endif
018000000616     C*
018100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018200000616     C                   movel(p)  CMD(2)        depcmd
018300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
018400000616     C*
018500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
018600011113     C                   Z-ADD     150           LENGH            15 5
018700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    depcmd
018900000616     C                   PARM                    LENGH
019000000616     C*
019100090930     C                   if        not %open(titv1r5ps)
019200090930     C                   open      titv1r5ps
019300000616     C                   except    testdett
019400000613     C                   endif
019500000613     C*
019600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
019700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
019800000613     C*
019900010202     C* Effettuo la chiamata al CLLE preposto
020000090114     C                   call(e)   'TITVVTC'
020100010202     C                   parm                    parccm
020200010202     C                   parm                    parmbr
020300010202     C                   parm      '2'           paropz
020400050201     C*
020500050201     C* Effettuo lancio TISI95 solo x chiusura
020600050201     C                   CLEAR                   TISI95DS
020700050201     C                   EVAL      I95TLA = 'C'
020800050201     C                   CALL      'TISI95R'
020900050201     C                   PARM                    TISI95DS
021000010202     C*
021100090930     C                   if        %open(titv1r5ps)
021200000616     C                   except    findett
021300090930     C                   close     titv1r5ps
021400000613     C                   endif
021500000616     C*
021600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
021700011113     C                   Z-ADD     150           LENGH            15 5
021800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021900000616     C                   PARM                    CMD(3)
022000000616     C                   PARM                    LENGH
022100000616     C*
022200000801     C
022300010201     C                   seton                                        LR
022400000613
022500000613
022600000613     C*--------------------------------------------------------
022700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
022800000613     C*--------------------------------------------------------
022900000621     C     STPR          BEGSR
023000000613     C*
023100090930     C                   if        not %open(titv1r5p)
023200090930     C                   open      titv1r5p
023300990915     C                   endif
023400990915     C*
023500990915     C                   except    riepilogo
023600990915     C*
023700090930     C                   if        %open(titv1r5p)
023800090930     C                   close     titv1r5p
023900990914     C                   endif
024000990910     C*
024100000613     C                   ENDSR
024200000613     C***
024300990908
024400000801
024500910830     C*--------------------------------------------------------
024600090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
024700910830     C*--------------------------------------------------------
024800040526     C     RWFILE        BEGSR
024900990910     C*
025000990914     C                   if        not %open(tivin00r)
025100990908     C                   open      tivin00r
025200990914     C                   endif
025300090114     C                   if        not %open(fivabwwr)
025400090114     C                   open      fivabwwr
025500990914     C                   endif
025600070103     C*
025700090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
025800020305     C                   exsr      prevat
025900010201     C*
026000010202     C                   if        chkcall = '0'
026100010202     C*
026200090114     C                   if        not %open(fivatwwr)
026300090114     C                   open      fivatwwr
026400010201     C                   endif
026500990910     C*
026600010201     C                   clear                   §CTROKVB          5 0
026700020305     C                   clear                   §CTROKVT          5 0
026800000801     C                   clear                   §CTRMO            5 0
026900000801     C                   clear                   §CTRNO            5 0
027000990910     C*
027100921023     C                   DO        *HIVAL
027200990913     C*
027300990915     C                   READ      tivin00r                               70
027400050627     C                   if        vindta > *blanks
027500000613     C                   add       1             rrnum
027600000801     C*
027700000801     C                   if        *in70 = *off
027800000801     C                             and
027900000801     C                             (vinflg = *blanks
028000000801     C                              or vinflg = '0'
028100000801     C                              or vinflg = '2')
028200000801     C*
028300000801     C                   clear                   vinmsg
028400000801     C                   eval      vinflg = '1'
028500070103     C*
028600070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
028700090930     C                   eval      curSped = %subst(vindta:277:15)
028800090930     C                   if        curSped <> depSped
028900070213     C* Se prima bolla => importo bolla corrente
029000091001     C                   if        §CTROKVB = *zeros
029100090309     C                   EXSR      INZVAR
029200090309     C                   EXSR      DEFCAM
029300070213     C                   exsr      impvab                                       => carico  VAB
029400070213     C                   else
029500070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
029600070213     C                   exsr      wrivab                                       => scarico VAB
029700090309     C                   EXSR      INZVAR
029800090309     C                   EXSR      DEFCAM
029900070213     C                   exsr      impvab                                       => carico  VAB
030000070213     C                   endif
030100090930     C                   exsr      exevate                                      => write VAT-E
030200070103     C* Salvo il raggruppamento spedizione cliente corrente
030300070103     C                   eval      depSped = curSped
030400070213     C*
030500070213     C* Se collo successivo x stessa bolla
030600070213     C                   else
030700070213     C                   exsr      impvab                                       => carico  VAB
030800090930     C                   exsr      exevate                                      => write VAT-E
030900070103     C                   endif
031000000905     C*
031100000905     C                   else
031200000905     C                   eval      vinflg = '1'
031300050628     C                   endif
031400000905     C                   endif
031500000905     C*
031600000905     C  N70              update    tivin000
031700000905     C*
031800991022     C  N70              ENDdo
031900070213     C*
032000070213     C* Scarico testata bolla rimasta in sospesa
032100070213     C                   exsr      wrivab                                       => scarico VAB
032200010202     C*
032300010202     C                   endif
032400990910
032500990910     C* Se non ci sono record con errori ...
032600000710     C                   if        §ctrno = 0
032700990910     C* ... restituisco esito OK.
032800990921     C                   eval      wrkesito = '0'
032900990910     C                   else
033000010201     C                   if        §ctrokvb > 0
033100990921     C                   eval      wrkesito = '1'
033200000710     C                   else
033300000710     C                   eval      wrkesito = '2'
033400990910     C                   endif
033500000710     C                   endif
033600990910     C*
033700990914     C                   if        %open(tivin00r)
033800990908     C                   close     tivin00r
033900990914     C                   endif
034000090114     C                   if        %open(fivabwwr)
034100090114     C                   close     fivabwwr
034200990914     C                   endif
034300090114     C                   if        %open(fivatwwr)
034400090114     C                   close     fivatwwr
034500010201     C                   endif
034600990910     C*
034700010201     C                   if        §ctrokvb > 0
034800000724     C                             and vlrpoi <> *zeros
034900010202     C                   exsr      invio
035000990920     C                   endif
035100990920     C*
035200910830     C                   ENDSR
035300000613     C***
035400010305
035500010305     C*----------------------------------------------------*
035600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035700010305     C*----------------------------------------------------*
035800020305     C     WRIVAB        BEGSR
035900010305     C*
036000060225     C* Quindi scarico il buffer del file d testata
036100090114     C                   write     fivab000                                     => scarico il VAB
036200010305     C*
036300010305     C                   ENDSR
036400990920
036500000801     C*----------------------------------------------------*
036600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036700000801     C*----------------------------------------------------*
036800010201     C     INZVAR        BEGSR
036900000801     C*
037000090114     C                   CLEAR                   FIVAB000
037100090114     C                   CLEAR                   FIVAT000
037200070213     C*
037300040802     C                   Z-ADD     *zeros        Num5_0            5 0
037400040802     C                   MOVEL     '0'           FlgCAS            1
037500000801     C*
037600000801     C                   ENDSR
037700000801     C*----------------------------------------------------*
037800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037900000801     C*----------------------------------------------------*
038000000801     C     DEFCAM        BEGSR
038100000801     C*
038200020619     C* Imposto i valori di default...
038300090930     C                   EVAL      VABCCM = 1050762
038400090930     C                   EVAL      VATCCM = 1050762
038500090930     C                   EVAL      VABLNP = 105
038600090930     C                   EVAL      VATLNP = 105
038700090930     C                   EVAL      VABNRS = 00
038800090930     C                   EVAL      VATNRS = 00
038900090930     C                   EVAL      VABCTR = 000
039000090930     C                   EVAL      VABTSP = 'D'
039100090930     C                   EVAL      VABCBO = '1'
039200090930     C                   EVAL      VABCTM = '7Q'
039300090930     C                   EVAL      VABTC1 = 'A'
039400020619     C* ... e poi verifico se sono stati passati come parametri
039500020619     C                   IF        vlrppt > *blanks
039600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
039700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039800020619     C                   EXSR      CHKNUM
039900020619     C                   IF        PiInt=*on
040000020619     C                   Z-ADD     PiVal         VABCCM
040100020619     C                   Z-ADD     PiVal         VATCCM
040200020619     C                   ENDIF
040300040506     C                   ENDIF
040400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
040500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040600020619     C                   EXSR      CHKNUM
040700020619     C                   IF        PiInt=*on
040800020619     C                   Z-ADD     PiVal         VABLNP
040900020619     C                   Z-ADD     PiVal         VATLNP
041000040506     C                   ENDIF
041100020619     C                   ENDIF
041200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
041300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041400020619     C                   EXSR      CHKNUM
041500020619     C                   IF        PiInt=*on
041600020619     C                   Z-ADD     PiVal         VABCTR
041700040506     C                   ENDIF
041800020619     C                   ENDIF
041900020619     C                   ENDIF
042000000801     C*
042100000801     C                   ENDSR
042200000801     C*----------------------------------------------------*
042300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042400000801     C*----------------------------------------------------*
042500040823     C     IMPVAB        BEGSR
042600010305     C*
042700000801     C                   Z-ADD     *zeros        errore            1 0
042800000830     C                   MOVEL     datcor        VABAAS
042900020305     C                   MOVEL     datcor        VATAAS
043000040526     C                   MOVE      datcor        VABMGS
043100040823     C                   MOVE(P)   vlrpoi        VABFGS
043200040823     C                   MOVE(P)   vlrpoi        VATFGS
043300090930     C*
043400090930     C                   eval      x_rec  = vindta
043500090930     C*
043600090930     C                   testb     x'3F'         vhbatb                   02
043700090930     C                   if        *in02
043800090930     C                   seton                                        31
043900090930     C                   else
044000090930     C*
044100090930     C                   SETOFF                                       303132
044200090930     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
044300090930     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
044400090930     C*    OPPURE TUTTI BLANKS
044500090930     C*
044600110126     C*                  TESTN                   VHBLNP               30
044700110126     C* N30              TESTN                   VHBLNP                 30
044800110126     C* N30              TESTN                   VHBLNP                   30
044900110126     C*                  MOVE      VHBLNP        W001A             1
045000110126     C*    *IN30         IFEQ      *OFF
045100110126     C*    W001A         ORLT      '0'
045200110126     C*                  SETON                                        31
045300110126     C*                  eval      vinmsg = %trimr(vinmsg)
045400110126     C*                            + ' ' + 'VHBLNP'
045500110126     C*                  ELSE
045600110126     C*                  MOVE      VHBLNP        VABLNP
045700110126     C*    VABLNP        IFLE      *ZEROS
045800110126     C*                  SETON                                        31
045900110126     C*                  eval      vinmsg = %trimr(vinmsg)
046000110126     C*                            + ' ' + 'VHBLNP'
046100110126     C*                  ENDIF
046200110126     C*                  ENDIF
046300090930     C*
046400090930     C* Effettuo considerazioni x elaborazioni "multi-filiale"
046500090930     C                   eval      depfil = VABLNP
046600091001     C                   eval      invfil = VABLNP
046700090930     C                   exsr      repfil
046800090930     C                   if        depfil = invfil
046900090930     C                   if        vlrpoi = 999
047000090930     C                   MOVE(P)   invfil        VABFGS
047100090930     C                   else
047200090930     C                   MOVE(P)   vlrpoi        VABFGS
047300090930     C                   endif
047400090930     C*
047500110126     C*                  TESTN                   VHBCCM               30
047600110126     C* N30              TESTN                   VHBCCM                 30
047700110126     C* N30              TESTN                   VHBCCM                   30
047800110126     C*                  MOVE      VHBCCM        W001A             1
047900110126     C*    *IN30         IFEQ      *OFF
048000110126     C*    W001A         ORLT      '0'
048100110126     C*                  SETON                                        31
048200110126     C*                  eval      vinmsg = %trimr(vinmsg)
048300110126     C*                            + ' ' + 'VHBCCM'
048400110126     C*                  ELSE
048500110126     C*                  MOVE      VHBCCM        VABCCM
048600110126     C*    VABCCM        IFLE      *ZEROS
048700110126     C*                  SETON                                        31
048800110126     C*                  eval      vinmsg = %trimr(vinmsg)
048900110126     C*                            + ' ' + 'VHBCCM'
049000110126     C*                  ENDIF
049100110126     C*                  ENDIF
049200090930     C*
049300090930     C                   TESTN                   VHBAAS               30
049400090930     C  N30              TESTN                   VHBAAS                 30
049500090930     C  N30              TESTN                   VHBAAS                   30
049600090930     C                   MOVE      VHBAAS        W001A             1
049700090930     C     *IN30         IFEQ      *OFF
049800090930     C     W001A         ORLT      '0'
049900090930     C     W001A         ANDNE     *BLANK
050000090930     C                   SETON                                        32
050100090930     C                   MOVEL     datcor        VABAAS
050200090930     C                   eval      vinmsg = %trimr(vinmsg)
050300090930     C                             + ' ' + 'VHBAAS'
050400090930     C                   ELSE
050500090930     C                   MOVE      VHBAAS        VABAAS
050600090930     C                   ENDIF
050700090930     C*
050800090930     C                   TESTN                   VHBMGS               30
050900090930     C  N30              TESTN                   VHBMGS                 30
051000090930     C  N30              TESTN                   VHBMGS                   30
051100090930     C                   MOVE      VHBMGS        W001A             1
051200090930     C     *IN30         IFEQ      *OFF
051300090930     C     W001A         ORLT      '0'
051400090930     C     W001A         ANDNE     *BLANK
051500090930     C                   SETON                                        32
051600090930     C                   MOVE      '0101'        VABMGS
051700090930     C                   eval      vinmsg = %trimr(vinmsg)
051800090930     C                             + ' ' + 'VHBMGS'
051900090930     C                   ELSE
052000090930     C                   MOVE      VHBMGS        VABMGS
052100090930     C                   ENDIF
052200090930     C*
052300090930     C                   TESTN                   VHBNRS               30
052400090930     C  N30              TESTN                   VHBNRS                 30
052500090930     C  N30              TESTN                   VHBNRS                   30
052600090930     C                   MOVE      VHBNRS        W001A             1
052700090930     C     *IN30         IFEQ      *OFF
052800090930     C     W001A         ORLT      '0'
052900090930     C     W001A         ANDNE     *BLANK
053000090930     C                   SETON                                        32
053100090930     C                   MOVE      *ZERO         VABNRS
053200090930     C                   eval      vinmsg = %trimr(vinmsg)
053300090930     C                             + ' ' + 'VHBNRS'
053400090930     C                   ELSE
053500090930     C                   MOVE      VHBNRS        VABNRS
053600090930     C                   ENDIF
053700090930     C*
053800090930     C                   TESTN                   VHBNSP               30
053900090930     C  N30              TESTN                   VHBNSP                 30
054000090930     C  N30              TESTN                   VHBNSP                   30
054100090930     C                   MOVE      VHBNSP        W001A             1
054200090930     C     *IN30         IFEQ      *OFF
054300090930     C     W001A         ORLT      '0'
054400090930     C     W001A         ANDNE     *BLANK
054500090930     C                   SETON                                        32
054600090930     C                   MOVE      *ZERO         VABNSP
054700090930     C                   eval      vinmsg = %trimr(vinmsg)
054800090930     C                             + ' ' + 'VHBNSP'
054900090930     C                   ELSE
055000090930     C                   MOVE      VHBNSP        VABNSP
055100090930     C                   ENDIF
055200090930     C*
055300090930     C                   TESTN                   VHBLNA               30
055400090930     C  N30              TESTN                   VHBLNA                 30
055500090930     C  N30              TESTN                   VHBLNA                   30
055600090930     C                   MOVE      VHBLNA        W001A             1
055700090930     C     *IN30         IFEQ      *OFF
055800090930     C     W001A         ORLT      '0'
055900090930     C     W001A         ANDNE     *BLANK
056000090930     C                   SETON                                        32
056100090930     C                   MOVE      *ZERO         VABLNA
056200090930     C                   eval      vinmsg = %trimr(vinmsg)
056300090930     C                             + ' ' + 'VHBLNA'
056400090930     C                   ELSE
056500090930     C                   MOVE      VHBLNA        VABLNA
056600090930     C                   ENDIF
056700090930     C*
056800090930     C                   TESTN                   VHBCTR               30
056900090930     C  N30              TESTN                   VHBCTR                 30
057000090930     C  N30              TESTN                   VHBCTR                   30
057100090930     C                   MOVE      VHBCTR        W001A             1
057200090930     C     *IN30         IFEQ      *OFF
057300090930     C     W001A         ORLT      '0'
057400090930     C     W001A         ANDNE     *BLANK
057500090930     C                   SETON                                        32
057600090930     C                   MOVE      *ZERO         VABCTR
057700090930     C                   eval      vinmsg = %trimr(vinmsg)
057800090930     C                             + ' ' + 'VHBCTR'
057900090930     C                   ELSE
058000090930     C                   MOVE      VHBCTR        VABCTR
058100090930     C                   ENDIF
058200090930     C*
058300090930     C                   MOVE      *ZERO         VABIAS
058400090930     C                   MOVE      VHBIAS        WASC15
058500090930     C                   TESTN                   WI15                 30
058600090930     C  N30              TESTN                   WI15                   30
058700090930     C  N30              TESTN                   WI15                     30
058800090930     C                   MOVE      WI15          W001A             1
058900090930     C     *IN30         IFEQ      *OFF
059000090930     C     W001A         ORLT      '0'
059100090930     C     W001A         ANDNE     *BLANK
059200090930     C                   SETON                                        32
059300090930     C                   MOVE      *ZERO         VABIAS
059400090930     C                   eval      vinmsg = %trimr(vinmsg)
059500090930     C                             + ' ' + 'VHBIAS'
059600090930     C                   ELSE
059700090930     C                   MOVEL     WI15          VABIAS
059800090930     C                   TESTN                   WD15                 30
059900090930     C  N30              TESTN                   WD15                   30
060000090930     C  N30              TESTN                   WD15                     30
060100090930     C                   MOVE      WD15          W001A             1
060200090930     C     *IN30         IFEQ      *OFF
060300090930     C     W001A         ORLT      '0'
060400090930     C     W001A         ANDNE     *BLANK
060500090930     C                   SETON                                        32
060600090930     C                   MOVE      *ZERO         VABIAS
060700090930     C                   eval      vinmsg = %trimr(vinmsg)
060800090930     C                             + ' ' + 'VHBIAS'
060900090930     C                   ELSE
061000090930     C                   MOVE      WD15          VABIAS
061100090930     C                   ENDIF
061200090930     C                   ENDIF
061300090930     C*
061400090930     C                   TESTN                   VHBNCL               30
061500090930     C  N30              TESTN                   VHBNCL                 30
061600090930     C  N30              TESTN                   VHBNCL                   30
061700090930     C                   MOVE      VHBNCL        W001A             1
061800090930     C     *IN30         IFEQ      *OFF
061900090930     C     W001A         ORLT      '0'
062000090930     C                   SETON                                        32
062100090930     C                   MOVE      *ZERO         VABNCL
062200090930     C                   eval      vinmsg = %trimr(vinmsg)
062300090930     C                             + ' ' + 'VHBNCL'
062400090930     C                   ELSE
062500090930     C                   MOVE      VHBNCL        VABNCL
062600090930     C                   ENDIF
062700090930     C*
062800090930     C                   MOVE      *ZERO         VABPKB
062900090930     C                   MOVE      VHBPKB        WASC09
063000090930     C                   IF        WI09 = *blanks
063100090930     C                   EVAL      WI09 = *zeros
063200090930     C                   ENDIF
063300090930     C                   TESTN                   WI09                 30
063400090930     C  N30              TESTN                   WI09                   30
063500090930     C  N30              TESTN                   WI09                     30
063600090930     C                   MOVE      WI09          W001A             1
063700090930     C     *IN30         IFEQ      *OFF
063800090930     C     W001A         ORLT      '0'
063900090930     C                   SETON                                        32
064000090930     C                   MOVE      *ZERO         VABPKB
064100090930     C                   eval      vinmsg = %trimr(vinmsg)
064200090930     C                             + ' ' + 'VHBPKB'
064300090930     C                   ELSE
064400090930     C                   MOVEL     WI09          VABPKB
064500090930     C                   TESTN                   WD09                 30
064600090930     C  N30              TESTN                   WD09                     30
064700090930     C                   MOVE      WD09          W001A             1
064800090930     C     *IN30         IFEQ      *OFF
064900090930     C     W001A         ORLT      '0'
065000090930     C     W001A         ANDNE     *BLANK
065100090930     C                   SETON                                        32
065200090930     C                   MOVE      *ZERO         VABPKB
065300090930     C                   eval      vinmsg = %trimr(vinmsg)
065400090930     C                             + ' ' + 'VHBPKB'
065500090930     C                   ELSE
065600090930     C                   MOVE      WD09          VABPKB
065700090930     C                   ENDIF
065800090930     C                   ENDIF
065900090930     C*
066000090930     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
066100090930     C                   MOVE      *ZERO         VABVLB
066200090930     C                   MOVE      VHBVLB        WASC07
066300090930     C                   IF        WI07 = *blanks
066400090930     C                   EVAL      WI07 = *zeros
066500090930     C                   ENDIF
066600090930     C                   TESTN                   WI07                 30
066700090930     C  N30              TESTN                   WI07                   30
066800090930     C  N30              TESTN                   WI07                     30
066900090930     C                   MOVE      WI07          W001A             1
067000090930     C     *IN30         IFEQ      *OFF
067100090930     C     W001A         ORLT      '0'
067200090930     C     W001A         ANDNE     *BLANK
067300090930     C                   SETON                                        32
067400090930     C                   MOVE      *ZERO         VABVLB
067500090930     C                   eval      vinmsg = %trimr(vinmsg)
067600090930     C                             + ' ' + 'VHBVLB'
067700090930     C                   ELSE
067800090930     C                   MOVEL     WI07          VABVLB
067900090930     C                   TESTN                   WD07                 30
068000090930     C  N30              TESTN                   WD07                   30
068100090930     C  N30              TESTN                   WD07                     30
068200090930     C                   MOVE      WD07          W001A             1
068300090930     C     *IN30         IFEQ      *OFF
068400090930     C     W001A         ORLT      '0'
068500090930     C     W001A         ANDNE     *BLANK
068600090930     C                   SETON                                        32
068700090930     C                   MOVE      *ZERO         VABVLB
068800090930     C                   eval      vinmsg = %trimr(vinmsg)
068900090930     C                             + ' ' + 'VHBVLB'
069000090930     C                   ELSE
069100090930     C                   MOVE      WD07          VABVLB
069200090930     C                   ENDIF
069300090930     C                   ENDIF
069400090930     C*
069500090930     C                   MOVE      *ZERO         VABQFT
069600090930     C                   MOVE      VHBQFT        WASC15
069700090930     C                   TESTN                   WI15                 30
069800090930     C  N30              TESTN                   WI15                   30
069900090930     C  N30              TESTN                   WI15                     30
070000090930     C                   MOVE      WI15          W001A             1
070100090930     C     *IN30         IFEQ      *OFF
070200090930     C     W001A         ORLT      '0'
070300090930     C     W001A         ANDNE     *BLANK
070400090930     C                   SETON                                        32
070500090930     C                   MOVE      *ZERO         VABQFT
070600090930     C                   eval      vinmsg = %trimr(vinmsg)
070700090930     C                             + ' ' + 'VHBQFT'
070800090930     C                   ELSE
070900090930     C                   MOVEL     WI15          VABQFT
071000090930     C                   TESTN                   WD15                 30
071100090930     C  N30              TESTN                   WD15                   30
071200090930     C  N30              TESTN                   WD15                     30
071300090930     C                   MOVE      WD15          W001A             1
071400090930     C     *IN30         IFEQ      *OFF
071500090930     C     W001A         ORLT      '0'
071600090930     C     W001A         ANDNE     *BLANK
071700090930     C                   SETON                                        32
071800090930     C                   MOVE      *ZERO         VABQFT
071900090930     C                   eval      vinmsg = %trimr(vinmsg)
072000090930     C                             + ' ' + 'VHBQFT'
072100090930     C                   ELSE
072200090930     C                   MOVE      WD15          VABQFT
072300090930     C                   ENDIF
072400090930     C                   ENDIF
072500090930     C*
072600090930     C                   MOVE      *ZERO         VABCAS
072700090930     C                   MOVE      VHBCAS        WASC15
072800090930     C                   TESTN                   WI15                 30
072900090930     C  N30              TESTN                   WI15                   30
073000090930     C  N30              TESTN                   WI15                     30
073100090930     C                   MOVE      WI15          W001A             1
073200090930     C     *IN30         IFEQ      *OFF
073300090930     C     W001A         ORLT      '0'
073400090930     C     W001A         ANDNE     *BLANK
073500090930     C                   SETON                                        32
073600090930     C                   MOVE      *ZERO         VABCAS
073700090930     C                   eval      vinmsg = %trimr(vinmsg)
073800090930     C                             + ' ' + 'VHBCAS'
073900090930     C                   ELSE
074000090930     C                   MOVEL     WI15          VABCAS
074100090930     C                   TESTN                   WD15                 30
074200090930     C  N30              TESTN                   WD15                   30
074300090930     C  N30              TESTN                   WD15                     30
074400090930     C                   MOVE      WD15          W001A             1
074500090930     C     *IN30         IFEQ      *OFF
074600090930     C     W001A         ORLT      '0'
074700090930     C     W001A         ANDNE     *BLANK
074800090930     C                   SETON                                        32
074900090930     C                   MOVE      *ZERO         VABCAS
075000090930     C                   eval      vinmsg = %trimr(vinmsg)
075100090930     C                             + ' ' + 'VHBCAS'
075200090930     C                   ELSE
075300090930     C                   MOVE      WD15          VABCAS
075400090930     C                   ENDIF
075500090930     C                   ENDIF
075600090930     C*
075700090930     C                   TESTN                   VHBRMN               30
075800090930     C  N30              TESTN                   VHBRMN                 30
075900090930     C  N30              TESTN                   VHBRMN                   30
076000090930     C                   MOVE      VHBRMN        W001A             1
076100090930     C     *IN30         IFEQ      *OFF
076200090930     C     W001A         ORLT      '0'
076300090930     C                   SETON                                        32
076400090930     C                   MOVE      *ZERO         VABRMN
076500090930     C                   eval      vinmsg = %trimr(vinmsg)
076600090930     C                             + ' ' + 'VHBRMN'
076700090930     C                   ELSE
076800090930     C                   MOVE      VHBRMN        VABRMN
076900090930     C                   ENDIF
077000090930     C*
077100090930     C                   TESTN                   VHBNCD               30
077200090930     C  N30              TESTN                   VHBNCD                 30
077300090930     C  N30              TESTN                   VHBNCD                   30
077400090930     C                   MOVE      VHBNCD        W001A             1
077500090930     C     *IN30         IFEQ      *OFF
077600090930     C     W001A         ORLT      '0'
077700090930     C     W001A         ANDNE     *BLANK
077800090930     C                   SETON                                        32
077900090930     C                   MOVE      *ZERO         VABNCD
078000090930     C                   eval      vinmsg = %trimr(vinmsg)
078100090930     C                             + ' ' + 'VHBNCD'
078200090930     C                   ELSE
078300090930     C                   MOVE      VHBNCD        VABNCD
078400090930     C                   ENDIF
078500090930     C*
078600090930     C                   TESTN                   VHBNCA               30
078700090930     C  N30              TESTN                   VHBNCA                 30
078800090930     C  N30              TESTN                   VHBNCA                   30
078900090930     C                   MOVE      VHBNCA        W001A             1
079000090930     C     *IN30         IFEQ      *OFF
079100090930     C     W001A         ORLT      '0'
079200090930     C     W001A         ANDNE     *BLANK
079300090930     C                   SETON                                        32
079400090930     C                   MOVE      *ZERO         VABNCA
079500090930     C                   eval      vinmsg = %trimr(vinmsg)
079600090930     C                             + ' ' + 'VHBNCA'
079700090930     C                   ELSE
079800090930     C                   MOVE      VHBNCA        VABNCA
079900090930     C                   ENDIF
080000090930     C*
080100090930     C                   TESTN                   VHBZNC               30
080200090930     C  N30              TESTN                   VHBZNC                 30
080300090930     C  N30              TESTN                   VHBZNC                   30
080400090930     C                   MOVE      VHBZNC        W001A             1
080500090930     C     *IN30         IFEQ      *OFF
080600090930     C     W001A         ORLT      '0'
080700090930     C     W001A         ANDNE     *BLANK
080800090930     C                   SETON                                        32
080900090930     C                   MOVE      *ZERO         VABZNC
081000090930     C                   eval      vinmsg = %trimr(vinmsg)
081100090930     C                             + ' ' + 'VHBZNC'
081200090930     C                   ELSE
081300090930     C                   MOVE      VHBZNC        VABZNC
081400090930     C                   ENDIF
081500090930     C*
081600090930     C                   TESTN                   VHBDCR               30
081700090930     C  N30              TESTN                   VHBDCR                 30
081800090930     C  N30              TESTN                   VHBDCR                   30
081900090930     C                   MOVE      VHBDCR        W001A             1
082000090930     C     *IN30         IFEQ      *OFF
082100090930     C     W001A         ORLT      '0'
082200090930     C     W001A         ANDNE     *BLANK
082300090930     C                   SETON                                        32
082400090930     C                   MOVE      *ZERO         VABDCR
082500090930     C                   eval      vinmsg = %trimr(vinmsg)
082600090930     C                             + ' ' + 'VHBDCR'
082700090930     C                   ELSE
082800090930     C                   MOVE      VHBDCR        VABDCR
082900090930     C                   ENDIF
083000090930     C*
083100090930     C                   TESTN                   VHBHCR               30
083200090930     C  N30              TESTN                   VHBHCR                 30
083300090930     C  N30              TESTN                   VHBHCR                   30
083400090930     C                   MOVE      VHBHCR        W001A             1
083500090930     C     *IN30         IFEQ      *OFF
083600090930     C     W001A         ORLT      '0'
083700090930     C     W001A         ANDNE     *BLANK
083800090930     C                   SETON                                        32
083900090930     C                   MOVE      *ZERO         VABHCR
084000090930     C                   eval      vinmsg = %trimr(vinmsg)
084100090930     C                             + ' ' + 'VHBHCR'
084200090930     C                   ELSE
084300090930     C                   MOVE      VHBHCR        VABHCR
084400090930     C                   ENDIF
084500090930     C*
084600090930     C                   MOVE      *ZERO         VABVMD
084700090930     C                   MOVE      VHBVMD        WASC15
084800090930     C                   TESTN                   WI15                 30
084900090930     C  N30              TESTN                   WI15                   30
085000090930     C  N30              TESTN                   WI15                     30
085100090930     C                   MOVE      WI15          W001A             1
085200090930     C     *IN30         IFEQ      *OFF
085300090930     C     W001A         ORLT      '0'
085400090930     C     W001A         ANDNE     *BLANK
085500090930     C                   SETON                                        32
085600090930     C                   MOVE      *ZERO         VABVMD
085700090930     C                   eval      vinmsg = %trimr(vinmsg)
085800090930     C                             + ' ' + 'VHBVMD'
085900090930     C                   ELSE
086000090930     C                   MOVEL     WI15          VABVMD
086100090930     C                   TESTN                   WD15                 30
086200090930     C  N30              TESTN                   WD15                   30
086300090930     C  N30              TESTN                   WD15                     30
086400090930     C                   MOVE      WD15          W001A             1
086500090930     C     *IN30         IFEQ      *OFF
086600090930     C     W001A         ORLT      '0'
086700090930     C     W001A         ANDNE     *BLANK
086800090930     C                   SETON                                        32
086900090930     C                   MOVE      *ZERO         VABVMD
087000090930     C                   eval      vinmsg = %trimr(vinmsg)
087100090930     C                             + ' ' + 'VHBVMD'
087200090930     C                   ELSE
087300090930     C                   MOVE      WD15          VABVMD
087400090930     C                   ENDIF
087500090930     C                   ENDIF
087600090930     C*
087700090930     C                   TESTN                   VHBANT               30
087800090930     C  N30              TESTN                   VHBANT                 30
087900090930     C  N30              TESTN                   VHBANT                   30
088000090930     C                   MOVE      VHBANT        W001A             1
088100090930     C     *IN30         IFEQ      *OFF
088200090930     C     W001A         ORLT      '0'
088300090930     C     W001A         ANDNE     *BLANK
088400090930     C                   SETON                                        32
088500090930     C                   MOVE      *ZERO         VABANT
088600090930     C                   eval      vinmsg = %trimr(vinmsg)
088700090930     C                             + ' ' + 'VHBANT'
088800090930     C                   ELSE
088900090930     C                   MOVE      VHBANT        VABANT
089000090930     C                   ENDIF
089100090930     C*
089200090930     C* CAMPI ALFABETICI:
089300090930     C                   MOVE      VHBATB        VABATB
089400090930     C                   MOVE      VHBCBO        VABCBO
089500090930     C                   MOVE      VHBRSD        VABRSD
089600090930     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089700090930     C     '@':'A'       XLATE     VABRSD        VABRSD
089800090930     C* ==
089900090930     C                   MOVE      VHBRD2        VABRD2
090000090930     C                   MOVE      VHBIND        VABIND
090100090930     C                   MOVE      VHBCAD        VABCAD
090200090930     C                   MOVE      VHBLOD        VABLOD
090300090930     C                   MOVE      VHBPRD        VABPRD
090400090930     C                   MOVE      VHBNZD        VABNZD
090500090930     C                   MOVE      VHBGC1        VABGC1
090600090930     C                   MOVE      VHBGC2        VABGC2
090700091105     C***                MOVE      VHBTSP        VABTSP
090800090930     C                   MOVE      VHBVAS        VABVAS
090900090930     C                   MOVE      VHBNAS        VABNAS
091000090930     C                   MOVE      VHBTIC        VABTIC
091100090930     C                   MOVE      VHBVCA        VABVCA
091200090930     C                   MOVE      VHBGCA        VABGCA
091300090930     C                   MOVE      VHBRMA        VABRMA
091400090930     C                   MOVE      VHBXCO        VABXCO
091500090930     C                   MOVE      VHBNOT        VABNOT
091600090930     C                   MOVE      VHBNT2        VABNT2
091700090930     C                   MOVE      VHBCTM        VABCTM
091800090930     C                   MOVE      VHBFFD        VABFFD
091900090930     C                   MOVE      VHBTCR        VABTCR
092000090930     C                   MOVE      VHBCTS        VABCTS
092100090930     C                   MOVE      VHBFTM        VABFTM
092200090930     C                   MOVE      VHBVAD        VABVAD
092300090930     C                   MOVE      VHBGMA        VABGMA
092400090930     C                   MOVE      VHBGGA        VABGGA
092500090930     C                   MOVE      VHBGVA        VABGVA
092600090930     C                   MOVE      VHBTC1        VABTC1
092700090930     C                   MOVE      VHBTC2        VABTC2
092800090930     C                   MOVE      VHBSCL        VABSCL
092900090930     C                   MOVE      VHBRMO        VABRMO
093000090930     C                   MOVE      VHBCMO        VABCMO
093100090930     C                   MOVE      VHBNMO        VABNMO
093200090930     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
093300090930     C* NELLA NAZIONE MITTENTE ORIGINALE
093400090930     C                   IF        %trim(VHBNMO) < 'A'
093500090930     C                   MOVE      *BLANKS       VABNMO
093600090930     C                   ENDIF
093700090930     C*
093800090930     C* Apporto le personalizzazioni cliente
093900110126     C***                EVAL      VABCCM = 1050762
094000110126     C***                EVAL      VATCCM = 1050762
094100110126     C***                EVAL      VABLNP = 105
094200110126     C***                EVAL      VATLNP = 105
094300090930     C                   EVAL      VABNRS = 00
094400090930     C                   EVAL      VATNRS = 00
094500090930     C                   EVAL      VABCTR = 000
094600090930     C                   EVAL      VABTSP = 'D'
094700090930     C                   EVAL      VABCBO = '1'
094800090930     C                   EVAL      VABCTM = '7Q'
094900090930     C                   Z-ADD     VABRMN        VABNSP
095000100215     C                   Z-ADD     VABRMN        VATNSP
095100091105     C                   IF        %subst(vindta:195:1) = '1'
095200091105     C                   EVAL      VABTC1 = 'A'
095300091105     C                   ENDIF
095400090930     C*
095500090930     C* Considerazioni sul contenuto di campi precedentemente valorizzati
095600090930     C                   IF        VABCAS  > *zeros
095700090930     C                   IF        VABCBO = '1'
095800090930     C                   EVAL      VABCBO = '4'
095900090930     C                   ENDIF
096000090930     C                   IF        VABCBO = '2'
096100090930     C                   EVAL      VABCBO = '6'
096200090930     C                   ENDIF
096300090930     C                   ENDIF
096400090930     C*
096500090930     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
096600090930     C                   IF        %trim(VABNZD) = 'I'   OR
096700090930     C                             %trim(VABNZD) = 'IT'  OR
096800090930     C                             %trim(VABNZD) = 'ITA'
096900090930     C                   EVAL      VABNZD = *blanks
097000090930     C                   ENDIF
097100090930     C*
097200090930     C* Reperisco la provincia dal CAP e dalla località
097300090930     C                   IF        VABCAD <> *blanks AND
097400090930     C                             VABPRD  = *blanks AND
097500090930     C                             VABNZD  = *blanks
097600090930     C                   CLEAR                   TISI95DS
097700090930     C                   EVAL      I95TCN = '3'
097800090930     C                   Z-ADD     datcor        I95DAT
097900090930     C                   EVAL      I95CAP = VABCAD
098000090930     C                   EVAL      I95LOC = VABLOD
098100090930     C                   CALL      'TISI95R'
098200090930     C                   PARM                    TISI95DS
098300090930     C                   EVAL      VABPRD = O95PRV
098400090930     C                   ENDIF
098500071121     C*
098600080415     C* Carico l'estensioni A e B del FNVAT
098700090930     C                   exsr      exevata
098800090930     C                   exsr      exevatb
098900020305     C*
099000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
099100011113     C                   EXSR      CHKIMPDIV
099200090930     C*
099300090930     C                   ENDIF
099400090930     C                   ENDIF
099500010202     C*
099600000801     C* Ebbene...
099700091001     C   32              ADD       1             §CTRMO
099800090930     C                   IF        *IN31
099900000801     C                   ADD       1             §CTRNO
100000000801     C                   EVAL      recko = vindta
100100000801     C                   EXCEPT    rigadett
100200000801     C                   EVAL      vinflg = '2'
100300000801     C                   ELSE
100400010201     C                   ADD       1             §CTROKVB
100500000801     C                   ENDIF
100600000801     C*
100700000801     C                   ENDSR
100800070102     C*----------------------------------------------------*
100900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
101000070102     C*----------------------------------------------------*
101100070103     C     EXEVATE       BEGSR
101200070102     C*
101300090428     C*
101400090930     C                   exsr      wrivat                                       => scarico VAT
101500070102     C*
101600070102     C                   ENDSR
101700080415     C*----------------------------------------------------*
101800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
101900080415     C*----------------------------------------------------*
102000080415     C     EXEVATA       BEGSR
102100080415     C*
102200090504     C                   EVAL      VATTRC = 'A'
102300090930     C                   EVAL      VATNOT = %trim(%subst(vindta:534:20))
102400080415     C*
102500090922     C                   exsr      wrivat                                       => scarico VAT
102600080415     C*
102700080415     C                   ENDSR
102800071121     C*----------------------------------------------------*
102900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
103000071121     C*----------------------------------------------------*
103100071121     C     EXEVATB       BEGSR
103200071121     C*
103300090504     C                   EVAL      VATTRC = 'B'
103400090930     C                   EVAL      VATNOT = %trim(%subst(vindta:554:20))
103500071121     C*
103600090922     C                   exsr      wrivat                                       => scarico VAT
103700071121     C*
103800071121     C                   ENDSR
103900010201     C*----------------------------------------------------*
104000090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
104100010201     C*----------------------------------------------------*
104200020305     C     WRIVAT        BEGSR
104300050628     C*
104400060223     C* Scrivo solo se valorizzato qualcosa
104500060223     C                   IF        VATNOT <> *blanks
104600090922     C                   WRITE     FIVAT000
104700090922     C                   ADD       1             §ctrokvt
104800060223     C                   ENDIF
104900010201     C*
105000010201     C                   ENDSR
105100010202     C*----------------------------------------------------*
105200090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
105300010202     C*----------------------------------------------------*
105400020305     C     PREVAT        BEGSR
105500010202     C*
105600090114     C* Compongo il nome del membro da dare al FIVATWWR
105700010202     C                   eval      parmbr = vlrhdl
105800010202     C                   movel     'M'           parmbr
105900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
106000010202     C                   eval      paropz = '1'
106100010202     C* Effettuo la chiamata al CLLE preposto
106200090114     C                   call(e)   'TITVVTC'
106300010202     C                   parm                    parccm
106400010202     C                   parm                    parmbr
106500010202     C                   parm                    paropz
106600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
106700010202     C                   if        %error
106800010202     C                   movel     '1'           chkcall
106900010202     C                   else
107000010202     C                   movel     '0'           chkcall
107100010202     C                   endif
107200010202     C*
107300010202     C                   ENDSR
107400000801     C*----------------------------------------------------*
107500090930     C*  CONTROLLO NUMERICITA' CAMPI
107600000801     C*----------------------------------------------------*
107700000801     C     CHKNUM        BEGSR
107800081217     C*
107900081217     C                   IF        PiDecChr = *blanks
108000090922     C                   EVAL      PiDecChr = '.'
108100081217     C                   ENDIF
108200081217     C*
108300081217     C                   callp(e)  UBISNUM_Check(PiStr
108400081217     C                                          :PiDecChr
108500081217     C                                          :PiVal
108600081217     C                                          :PiNum
108700081217     C                                          :PiInt)
108800081217     C*
108900081217     C                   IF        %error
109000081217     C                   EVAL      PiInt=*off
109100081217     C                   ENDIF
109200000801     C*
109300000801     C                   ENDSR
109400000801     C***
109500000801
109600011113
109700011113     C*----------------------------------------------------*
109800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
109900011113     C*----------------------------------------------------*
110000011113     C     CHKIMPDIV     BEGSR
110100011113     C*
110200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
110300011113     C                   Z-ADD     *zeros        wrkDec            9 9
110400011113     C*
110500011113     C* Come prima cosa effettuo considerazioni sulla divisa
110600011113     C                   IF        vabIAS > *zeros
110700011113     C                   IF        vabVAS <> 'EUR'
110800011113     C                   EVAL      vabVAS =  'ITL'
110900011113     C                   ENDIF
111000011113     C                   ENDIF
111100011113     C*
111200011113     C                   IF        vabCAS > *zeros
111300011113     C                   IF        vabVCA <> 'EUR'
111400011113     C                   EVAL      vabVCA =  'ITL'
111500011113     C                   ENDIF
111600011113     C                   ENDIF
111700011113     C*
111800011113     C                   IF        vabVMD > *zeros
111900020305     C                   IF        vabVAD <> 'EUR'
112000011113     C                   EVAL      vabVAD =  'ITL'
112100011113     C                   ENDIF
112200011113     C                   ENDIF
112300011113     C*
112400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
112500011113     C                   Z-ADD     vabIAS        wrkDec
112600011113     C                   IF        wrkDec > *zeros
112700011113     C                   IF        vabVAS = 'ITL'
112800011113     C                   EVAL      vabIAS = *zeros
112900011113     C                   ENDIF
113000011113     C                   ENDIF
113100011113     C*
113200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
113300011113     C                   Z-ADD     vabCAS        wrkDec
113400011113     C                   IF        wrkDec > *zeros
113500011113     C                   IF        vabVCA = 'ITL'
113600011113     C                   EVAL      vabCAS = *zeros
113700011113     C                   ENDIF
113800011113     C                   ENDIF
113900011113     C*
114000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
114100011113     C                   Z-ADD     vabVMD        wrkDec
114200011113     C                   IF        wrkDec > *zeros
114300011113     C                   IF        vabVAD = 'ITL'
114400011113     C                   EVAL      vabVMD = *zeros
114500011113     C                   ENDIF
114600011113     C                   ENDIF
114700011113     C*
114800011113     C                   ENDSR
114900011113     C***
115000011113
115100011113
115200000801
115300000801
115400990920      /TITLE Invio dei dati al punto operativo.
115500010202     C     invio         BEGSR
115600990920     C*
115700090114     C* 1° invio FIVAT
115800010201     C                   reset                   dscmz
115900010201     C                   move      vlrpoi        cmzdst
116000090114     C                   eval      cmzfld = 'FIVATWWR'
116100010201     C                   eval      cmzmbd = vlrhdl
116200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
116300021009     C***                if        prmfir = *blanks
116400090114     C                   eval      cmzfla = 'FIVAT00F'
116500090114     C                   eval      cmzmba = 'FIVAT00F'
116600021009     C***                else
116700021009     C***                eval      cmzfla = prmfir
116800021009     C***                eval      cmzmba = prmfir
116900021009     C***                endif
117000010201     C                   eval      cmznrr = *zeros
117100020305     C                   move      §ctrokvt      cmznrr
117200021018     C                   eval      cmzlba = vlrfl1
117300010201     C                   call(e)   'TIS711C'
117400010201     C                   parm                    dscmz
117500010201     C                   parm      *blanks       esito
117600010205     C                   if        %error
117700010205     C                             or cmzerr = '1'
117800010205     C                             or esito  = '1'
117900010205     C                   eval      wrkesito = '3'
118000010205     C                   else
118100010201     C*
118200090114     C* 2° invio FIVAB
118300010201     C                   reset                   dscmz
118400010201     C                   move      vlrpoi        cmzdst
118500010201     C                   eval      cmzfld = vlrfou
118600010201     C                   eval      cmzmbd = vlrhdl
118700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
118800021009     C***                if        prmfir = *blanks
118900090114     C                   eval      cmzfla = 'FIVAB00F'
119000090114     C                   eval      cmzmba = 'FIVAB00F'
119100021009     C***                else
119200021009     C***                eval      cmzfla = prmfir
119300021009     C***                eval      cmzmba = prmfir
119400021009     C***                endif
119500010201     C                   eval      cmznrr = *zeros
119600010201     C                   move      §ctrokvb      cmznrr
119700021018     C                   eval      cmzlba = vlrfl1
119800010201     C                   call(e)   'TIS711C'
119900010201     C                   parm                    dscmz
120000010201     C                   parm      *blanks       esito
120100010201     C                   if        %error
120200010201     C                             or cmzerr = '1'
120300010201     C                             or esito  = '1'
120400010201     C                   eval      wrkesito = '3'
120500010201     C                   endif
120600010205     C                   endif
120700990920     C*
120800000613     C                   ENDSR
120900000613     C***
121000070411
121100090331
121200090331
121300090331
121400070411     C     *pssr         BEGSR
121500070411     C*
121600070411     C                   if        %open(tivin00r)
121700070411     C                   close     tivin00r
121800070411     C                   endif
121900090114     C                   if        %open(fivabwwr)
122000090114     C                   close     fivabwwr
122100070411     C                   endif
122200090114     C                   if        %open(fivatwwr)
122300090114     C                   close     fivatwwr
122400070411     C                   endif
122500070411     C*
122600070411     C* Effettuo la chiamata al CLLE preposto
122700090114     C                   call(e)   'TITVVTC'
122800070411     C                   parm                    parccm
122900070411     C                   parm                    parmbr
123000070411     C                   parm      '2'           paropz
123100070411     C*
123200070411     C                   eval      wrkesito = '2'
123300070411     C*
123400070411     C                   seton                                        LR
123500070411     C*
123600070411     C                   ENDSR     '*CANCL'
123700070411     C***
123800070411
123900090930
124000090930
124100090930      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
124200090930     C     repfil        BEGSR
124300090930     C*
124400090930     C                   if        invfil = *zeros and
124500090930     C                             depfil > *zeros and
124600090930     C                             (vinflg = *blanks or
124700090930     C                              vinflg = *zeros)
124800090930     C
124900090930     C                   eval      invfil = depfil
125000090930     C                   endif
125100090930     C*
125200090930     C                   if        depfil <> invfil and
125300090930     C                             invfil > *zeros
125400090930     C                   eval      flgMulti = '1'
125500090930     C                   if        vinflg = *blanks
125600090930     C                   add       1             cntNonEl
125700090930     C                   endif
125800090930     C                   endif
125900090930     C*
126000090930     C                   if        vinflg = '2'
126100090930     C                   eval      flgStato = '2'
126200090930     C                   endif
126300090930     C*
126400090930     C                   ENDSR
126500090930     C***
126600090930
126700090930
126800990910
126900000613     C     *inzsr        BEGSR
127000990910     C*
127100990910     C     *entry        plist
127200990920     C                   parm                    tivlrds
127300990921     C                   parm      wrkesito      esito
127400000724     C                   parm                    prmlit
127500000710     C                   parm                    prmfir
127600000613     C*
127700000830     C* CALCOLA LA DATA CORRENTE
127800000830     C                   time                    wn14             14 0
127900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
128000080512     C                   movel     wn14          oracor            6 0          *ORA
128100000830     C                   z-add     wn8           g08dat
128200000830     C                   z-add     *zeros        g08inv
128300000830     C                   movel     '0'           g08err
128400000830     C                   call      'XSRDA8'
128500000830     C                   parm                    wlbda8
128600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
128700000830     C*
128800000613     C                   ENDSR
128900000613     C***
129000990908
129100090331
129200090331
129300090930     Otitv1r5p  E            riepilogo         2
129400990915     O                                              'Upload via Internet'
129500990915     O                                           +1 'Traduzione TIVIN00R -'
129600090114     O                                           +1 'FIVABWWR/FIVATWWR'
129700010201     O                                           +1 'Report testate bolle'
129800990915     O          E            riepilogo   2
129900990915     O                       wrkdata
130000990915     O                       wrkora              +1
130100990915     O                       procname            +1
130200990915     O          E            riepilogo   2
130300990915     O                                              'Cliente..................:'
130400990915     O                       VABCCM        z     +1
130500990915     O          E            riepilogo   2
130600990920     O                                              'Riferimento Strategi.....:'
130700990920     O                       vlrhdl              +1
130800990915     O          E            riepilogo   2
130900990915     O                                              'Giusti...................:'
131000010201     O                       §CTROKVB      2   +  1
131100990915     O          E            riepilogo   2
131200990915     O                                              'Sbagliati e corretti.....:'
131300971022     O                       §CTRMO        2   +  1
131400990915     O          E            riepilogo   2
131500990915     O                                              'Sbagliati e scartati.....:'
131600971022     O                       §CTRNO        2   +  1
131700000613
131800090930     Otitv1r5ps E            testdett          2
131900000613     O                                              'Upload via Internet'
132000000613     O                                           +1 'Traduzione TIVIN00R -'
132100090114     O                                           +1 'FIVABWWR/FIVATWWR'
132200010201     O                                           +1 'Report testate bolle'
132300000616     O          E            testdett    3
132400000613     O                                           +2 'N° rec'
132500000613     O                                           +3 'Anteprima contenuto'
132600000616     O          E            rigadett    2
132700000613     O                       rrnum               +2
132800000621     O                       recko               +3
132900000616     O          E            findett     2
133000000613     O                       wrkdata
133100000613     O                       wrkora              +1
133200000613     O                       procname            +1
133300000616     O          E            findett     2
133400000613     O                                              'Cliente..................:'
133500000613     O                       VABCCM        z     +1
133600000616     O          E            findett     2
133700000613     O                                              'Riferimento Strategi.....:'
133800000613     O                       vlrhdl              +1
133900000616     O          E            findett     2
134000000613     O                                              'Giusti...................:'
134100010201     O                       §CTROKVB      2   +  1
134200000616     O          E            findett     2
134300000613     O                                              'Sbagliati e corretti.....:'
134400000613     O                       §CTRMO        2   +  1
134500000616     O          E            findett     2
134600000613     O                                              'Sbagliati e scartati.....:'
134700000613     O                       §CTRNO        2   +  1
134800000512** CMD - COMANDI CL
134900090930OVRPRTF FILE(TITV1R5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
135000090930OVRPRTF FILE(TITV1R5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
135100090930DLTOVR FILE(TITV1R5P TITV1R5PS) LVL(*)
135200000512
135300000512
