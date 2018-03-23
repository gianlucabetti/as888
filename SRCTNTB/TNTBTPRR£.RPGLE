000100020208     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020208      *--------------------------------------------------------------------------------------------*
000300160615      *     personalizzazioni ambiti di intradamento giri di ritiro Tellus                         *
000400020208      *--------------------------------------------------------------------------------------------*
000500020208      * Data base                                                                                  *
000600020208      *--------------------------------------------------------------------------------------------*
000700020208     FAzOrg01l  IF   E           K DISK
000800070126     FTntbe01l  uF A E           K Disk
000900160615     FTnTbTPrd  CF   E             WORKSTN
001000020208      *--------------------------------------------------------------------------------------------*
001100020208      * Data structure                                                                             *
001200020208      *--------------------------------------------------------------------------------------------*
001300020208      *---
001400020208      * Schiere memorizzazione dati correnti
001500020208      *---
001600020320     D msg             S             78    DIM(6) CTDATA PERRCD(1)              *Messaggi video
001700070126     d
001800070126     D og148         E DS                                                       *ds di procedura
001900160615     D DTPR          E DS                                                       *ds di procedura
002000020208      *
002100020208     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200070126     D fnlv50ds      E DS
002300020208     D kpjba         E DS                                                       *architettura
002400020208      *--------------------------------------------------------------------------------------------*
002500020208      * Main lines
002600020208      *--------------------------------------------------------------------------------------------*
002700020208      *
002800020208      * Operazioni iniziali
002900020208      *
003000020208     C                   EXSR      RutInz
003100020208      *
003200020208      * Gestione video
003300020208      *
003400020208     C     $Fine         DOUEQ     'S'
003500070126     c     TipVid        CASEQ     '1'           GesVd1                          *Sfile
003600020208     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile
003700020208     C                   ENDCS
003800020208     C                   ENDDO
003900020208      *
004000020208     C                   SETON                                        LR
004100020208      *--------------------------------------------------------------------------------------------*
004200020208      * GesVd1 - Gestione videata immissione nuova tabella -
004300020208      *--------------------------------------------------------------------------------------------*
004400020208     C     GesVd1        BEGSR
004500020208      *
004600020208      * Inizializzazione videata
004700070126     c                   clear                   v1cfil
004800070126     c                   clear                   v1dfil
004900020208      *
005000070126     c     for01         tag
005100070126     C                   EXFMT     Tb59v1
005200070126     c                   clear                   v1cmsg
005300070126     c                   setoff                                       2890
005400020208      *
005500070126     c* F3 -  fine
005600070126     c                   if        *inkc
005700070126     C                   EXSR      F03Ges
005800070126     c                   goto      endv1
005900070126     c                   endif
006000020208      *
006100020208      * Controlli e decodifiche
006200020208     C                   EXSR      CtrV1
006300070126     c* Errori
006400070126     c                   if        *in90
006500070126     c                   goto      for01
006600070126     c                   endif
006700070126     c
006800070126     c                   movel     '2'           tipvid            1
006900020208      *
007000070126     C     endv1         endsr
007100020208      *--------------------------------------------------------------------------------------------*
007200020208      * Controllo e decodifica prima videata
007300020208      *--------------------------------------------------------------------------------------------*
007400020208     C     CtrV1         BEGSR
007500020208      *
007600020214     C                   setoff                                       283940
007700020208     C                   clear                   V1CMsg
007800070126      * PUNTO OPERATIVO
007900070126     C                   MOVE      V1Cfil        CODFIL            3 0
008000070126     C                   EXSR      CTRFIL
008100070126    1c                   if        *in90
008200070126     c                   goto      endct1
008300070126    1C                   ENDIF
008400070126     c
008500070126     c* Verifico se  abilitato a scambio dati con tellus  se no EDP
008600070126     c                   if        not *in01
008700070126     c                   movel     orgde8        og148
008800070126     c                   if        §ogtlls=' '
008900070126     c                   seton                                        509028
009000070126     C                   MOVEL     MSG(2)        V1cMSG
009100070126     c                   goto      endct1
009200070126    1C                   ENDIF
009300070126    1C                   ENDIF
009400070126      *
009500070126     c
009600070126     C     endct1        ENDSR
009700070126     C**************************************************************************
009800070126     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
009900070126     C**************************************************************************
010000070126     C     CTRFIL        BEGSR
010100070126
010200070126     C     CODFIL        CHAIN     AZORG01L                           34
010300070126    1C  N34ORGFVA        IFNE      ' '
010400070126     C     ORGFAG        ORNE      'A'
010500070126     C     ORGFAG        ANDNE     'F'
010600070126     C                   SETON                                        34
010700070126    1C                   ENDIF
010800070126     c
010900070126    1c                   if        not *in34
011000070126     c                   movel     orgdes        V1Dfil
011100070126     c
011200070126   x1C                   ELSE
011300070126     c                   seton                                        509028
011400070126     C                   MOVEL     MSG(1)        V1cMSG
011500070126     c                   goto      endctf
011600070126    1C                   ENDIF
011700070126     c
011800070126     c* Se non sono EDP deve essere p.o. della £1
011900070126     c                   if        not *in01
012000070126     C                   SETOFF                                       90
012100070126     C                   CLEAR                   fnlv50ds
012200070126     C                   MOVEL     KNMUS         D50PRU
012300070126     C                   MOVEL     codFIL        D50FGS
012400070126     C                   CALL      'FNLV50R'
012500070126     C                   PARM                    fnlv50ds
012600070126     C*
012700070126     C     D50ERR        IFNE      ' '
012800070126     C                   MOVEL     D50MSG        V1CMSG
012900070126     C                   SETON                                        509028
013000070126     C                   GOTO      ENDCTF
013100070126     C                   ENDIF
013200070126     C                   ENDIF
013300070126     C*
013400070126     C     endctf        ENDSR
013500020208      *--------------------------------------------------------------------------------------------*
013600020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
013700020208      *--------------------------------------------------------------------------------------------*
013800020208     C     GesVd2        BEGSR
013900020208      *
014000020208     C                   EXSR      CarVd2
014100020208      *
014200070126     c     for02         tag
014300070126     C                   EXFMT     Tb59v2
014400070126     c
014500070126     c                   clear                   v1cmsg
014600070126     c                   setoff                                       2890
014700020208      *
014800070126     C* F3 - fine lavoro
014900070126     c                   if        *inkc
015000070126     C                   EXSR      F03Ges
015100070126     c                   goto      endv2
015200070126     c                   endif
015300070126      *
015400070126     C* F12 - ritorno
015500070126     c                   if        *inkl
015600070126     C                   move      '1'           TipVid
015700070126     c                   goto      endv2
015800070126     c                   endif
015900070126     c* F16 - annullamento
016000070126     c                   if        *inkq
016100070126     c*
016200070430     c* errore in immissione
016300070126     c                   if        not %found(tntbe01l)
016400070126     c                   movel     msg(4)        v1cmsg
016500070126     c                   seton                                        289041
016600070126     c                   goto      for02
016700070126     c                   else
016800070126     c                   delete    tntbe000
016900070126     C                   move      '1'           TipVid
017000070126     c                   goto      endv2
017100070126     c                   endif
017200070126     c                   endif
017300070126      *
017400020208     C                   EXSR      CtrV2
017500070126     c
017600070126     c                   if        *in90  or not *inkf
017700070126     c                   goto      for02
017800070126     c                   endif
017900070126     c
018000070126     c* f6 - Aggiorna
018100070126     c                   if        *inkf
018200070126     c                   exsr      Aggiorna
018300070126     c                   endif
018400070129     c
018500070129     c                   movel     '1'           tipvid            1
018600070129     c                   clear                   v1cfil
018700070129     c                   clear                   v1dfil
018800020211      *
018900070126     C     Endv2         endsr
019000020208      *--------------------------------------------------------------------------------------------*
019100020208      * CarVd2 - Carica primo video
019200020208      *--------------------------------------------------------------------------------------------*
019300020208     C     CarVd2        BEGSR
019400020208      *
019500020211      * Leggo la tabella
019600070126     C                   movel(P)  v1cfil        TbeKe1
019700020211     C                   movel     *blanks       TbeKe2
019800070126     C     ktbe          chain     tntbe01l                           21
019900070126     c
020000070126     c* Se trovata carico i dati
020100070126     c                   if        %found(tntbe01l)
020200070126     c                   movel     tbeuni        dtep
020300070126     c                   eval      v1cmod='VARIAZIONE '
020400070126     c                   else
020500070126     c                   clear                   dtep
020600070126     c                   eval      v1cmod='INSERIMENTO'
020700070126     c                   endif
020800070126     c
020900070126     c                   movel     §teplkg       v1clkg
021000070126     c                   movel     §teplmc       v1clmc
021100070126     c                   movel     §teplks       v1clks
021200070126     c                   movel     §teplms       v1clms
021300070524     c                   clear                   v1cais
021400070524     c
021500070524     c* Per maggior chiarezza a video l'ambito instradamento ai
021600070524     c*  supermercati:
021700070524     c*               - §tepsup=S  viene visualizzato con "SI"
021800070524     c*               - §tepsup=P  viene visualizzato con "NO'
021900070524     c*                            per applicare l'instradamento in base
022000070524     c*                            al peso o volume della bolla
022100070524     c*
022200070524     c                   if        §tepsup='S'
022300070524     c                   movel     'SI'          v1cais
022400070524     c                   endif
022500070524     c                   if        §tepsup='P'
022600070524     c                   movel     'NO'          v1cais
022700070524     c                   endif
022800070126     c
022900020208     C                   ENDSR
023000020208      *--------------------------------------------------------------------------------------------*
023100020215      * Controllo seconda videata
023200020208      *--------------------------------------------------------------------------------------------*
023300020208     C     CtrV2         BEGSR
023400070126     C                   setoff                                       289041
023500070126     C                   setoff                                       424344
023600070126     c* se immesso limite peso immettere anche limite volume e
023700070126     c*  viceversa
023800070430     c* Obbligatori tutti e 4
023900070430     c                   if        (v1clkg>0 and v1clmc>0 and v1clks>0
024000070430     c                             and v1clms>0) or
024100070430     c                             (v1clkg=0 and v1clmc=0 and v1clks=0
024200070430     c                             and v1clms=0)
024300070430     c                   else
024400070430     c*
024500070430     c     v1clkg        comp      0                                      43
024600070430     c     v1clmc        comp      0                                      44
024700070430     c     v1clks        comp      0                                      41
024800070430     c     v1clms        comp      0                                      42
024900070430     c                   seton                                          2890
025000070430     C                   MOVEL     MSG(3)        V1cMSG
025100070430     c                   goto      endct2
025200070430     c                   endif
025300070430     c**
025400070430     c**                 if        v1clkg>0 and v1clmc=0
025500070430     c**                 seton                                        442890
025600070430     C**                 MOVEL     MSG(3)        V1cMSG
025700070430     c**                 goto      endct2
025800070430     c**                 endif
025900070430     c**                 if        v1clkg=0 and v1clmc>0
026000070430     c**                 seton                                        432890
026100070430     C**                 MOVEL     MSG(3)        V1cMSG
026200070430     c**                 goto      endct2
026300070430     c**                 endif
026400070430     c**                 if        v1clks>0 and v1clms=0
026500070430     c**                 seton                                        422890
026600070430     C**                 MOVEL     MSG(3)        V1cMSG
026700070430     c**                 goto      endct2
026800070430     c**                 endif
026900070430     c**                 if        v1clks=0 and v1clms>0
027000070430     c**                 seton                                        412890
027100070430     C**                 MOVEL     MSG(3)        V1cMSG
027200070430     c**                 goto      endct2
027300070430     c**                 endif
027400070406     c* Se personalizza = ' ' --> errore
027500070524     c                   if        v1cais=' '
027600070406     c                   seton                                        459028
027700070406     C                   MOVEL     MSG(4)        V1cMSG
027800070406     c                   goto      endct2
027900070406    1C                   ENDIF
028000020214      *
028100070126     C     endct2        ENDSR
028200020208      *--------------------------------------------------------------------------------------------*
028300070126      * Aggiornamenti
028400020208      *--------------------------------------------------------------------------------------------*
028500070126     C     Aggiorna      BEGSR
028600020211      *
028700070126     C                   clear                   Tntbe000
028800020211     C                   move      ' '           TbeAtb
028900020211     C                   movel     $Sif          TbeSif
029000020211     C                   movel     $Lin          TbeLin
029100020211     C                   movel     $Apl          TbeApl
029200070126     C                   move      'TEP'         TbeCod
029300020211     C                   move      *blanks       TbeKe1
029400020211     C                   move      *blanks       TbeKe2
029500070126     c                   movel     v1cfil        tbeke1
029600070126     c* campi della DS
029700070126     c                   movel     v1clkg        §teplkg
029800070126     c                   movel     v1clmc        §teplmc
029900070126     c                   movel     v1clks        §teplks
030000070126     c                   movel     v1clms        §teplms
030100070524     c
030200070524     c* Per maggior chiarezza a video l'ambito instradamento ai
030300070524     c*  supermercati :
030400070524     c*               - §tepsup=S  viene visualizzato con "SI"
030500070524     c*               - §tepsup=P  viene visualizzato con "NO'
030600070524     c*                            per applicare l'instradamento in base
030700070524     c*                            al peso / volume della bolla
030800070524     c*
030900070524     c                   if        v1cais='NO'
031000070524     c                   movel     'P'           §tepsup
031100070524     c                   else
031200070524     c                   movel     'S'           §tepsup
031300070524     c                   endif
031400020211      *
031500070126     C                   movel(P)  dtep          TbeUni
031600020215      *
031700070126     C                   move      ' '           TbeFtt
031800020211     C                   z-add     *zeros        TbeFlt
031900020211     C                   move      ' '           TbeFtr
032000020211     C                   move      *zeros        TbeDtr
032100020211      *
032200070126     c                   if        %found(tntbe01l)
032300070126     c                   update    tntbe000
032400070126     c                   else
032500070126     C                   write     Tntbe000
032600070126     c                   endif
032700020211      *
032800020211     C                   ENDSR
032900020208      *--------------------------------------------------------------------------------------------*
033000020208      * F03ges - Tasto funzionale f03 -> Fine programma
033100020208      *--------------------------------------------------------------------------------------------*
033200020208     C     F03ges        BEGSR
033300070126     C                   movel     'S'           $Fine             1            *fine pgm
033400020208     C                   ENDSR
033500020208      *--------------------------------------------------------------------------------------------*
033600020208      * RutInz - operazioni iniziali
033700020208      *--------------------------------------------------------------------------------------------*
033800020208     C     rutinz        BEGSR
033900020208      *
034000020208      * Ricevimento parametri
034100020208     C     *ENTRY        PLIST
034200020208     C                   PARM                    kpjba
034300020208      *
034400020208     C                   z-add     1             CodUt
034500020208     C                   CALL      'X§PARUT'
034600020208     C                   PARM                    UT§DSE
034700020208     C                   move      '1'           TipVid
034800020211      *
034900020211      * Reperisco testata tabella
035000020211     C                   movel     *blanks       Tbecod
035100020211     C                   move      *zeros        TbeKe1
035200070126     C                   move      'TEP'         TbeKe1
035300020211     C     K01Tbe2       chain     Tntbe01l                           21
035400020211      *
035500020211      * Se non esiste l atestata tabella non faccio nulla
035600020211     C   21              move      '1'           $Fine
035700020211      *
035800020211     C  n21              do
035900020211     C                   movel     TbeSif        $Sif             10
036000020211     C                   movel     TbeApl        $Apl              2
036100020211     C                   movel     TbeLin        $Lin              1
036200070126     C                   move      'TEP'         TbeCod
036300020211     C                   enddo
036400070126     c
036500070126     c* EDP tutto è possibile
036600070126     c                   if        %subst(knmus:1:3)='EDP'
036700070126     c                   seton                                        01
036800070126     c                   endif
036900070126     c                   movel     ragut         v1crag
037000070126     c                   movel     knmus         v1cute
037100070126     c                   movel     knsif         v1csif
037200020208      *
037300020208      * chiavi di lettura
037400020208      *
037500020211     C     k01tbe2       KLIST                                                  *tntbe01l
037600020211     C                   KFLD                    tbecod                          -tabella
037700020211     C                   KFLD                    tbeke1                          -chiave uno
037800020212      *
037900070126     C     ktbe          KLIST                                                  *tntbe01l
038000020212     C                   KFLD                    TbeCod                          -tabella
038100020212     C                   KFLD                    tbeke1                          -chiave uno
038200020212     C                   KFLD                    tbeKe2                          -tabella
038300020212     C                   KFLD                    $lin                            -chiave uno
038400020212     C                   KFLD                    $sif                            -tabella
038500070126     c                   ENDSR
038600020212      *
038700020211      *
038800020212** Msg
038900020212Punto operativo inesistente o errato
039000070126Punto Operativo non abilitato allo scambio dati con TELLUS !!!
039100070126Se immesso limite peso, immettere anche limite volume e viceversa
039200070524Impostare la gestione ambito ai Supermercati uguale a "SI" oppure "NO"
