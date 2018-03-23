000100000000     h*--------------------------------------------------------------------------------------------*
000200170125     h* Trascodifica dati RPVP da FLTR5 a FITR4 (trak VDL)                                         *
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* Data base
000700000000     f*--------------------------------------------------------------------------------------------*
000800030716     fazorg01l  IF   E           K DISK
000900091125     ffltr5err  O  A E             DISK    prefix(e_)
001000030716     ffltr500f  UF   E             DISK
001100170125     ffitr400f  O  A E             DISK
001200000000     d*--------------------------------------------------------------------------------------------*
001300000000     d* Data structure
001400000000     d*--------------------------------------------------------------------------------------------*
001500000000     d*---
001600030716     d* Variabili di wrk
001700000000     d*---
001800030716     d flgOK           S              1
001900030716     d wPOn            S              3  0
002000030716     d wPOa            S              3
002100030716     d wBarcode        S             20
002200030718     d wVolume         S              7  0
002300030718     d wLatoCub        S             30  9
002400030721     d wLatoCubInt     S              4  0
002500030721     d wLatoCubIntA    S              4
002600030716     d wArrotonda1a    S              1
002700030716     d wArrotonda1n    S              1  0
002800030716     d wArrotonda2a    S              4
002900030716     d wArrotonda2n    S              4  0
003000000000     d*---
003100030716     d* DS ridefinizione file piatto d input da pc RPVP
003200000000     d*---
003300030716     d RPVP_DS         DS
003400030716     d  RPVP_TipApp                   1
003500030716     d  RPVP_ID                      30
003600030716     d  RPVP_PesoGr                   7
003700030716     d  RPVP_VolCm3                   7
003800030716     d  RPVP_LungMm                   5
003900030716     d  RPVP_AltezzMm                 5
004000030716     d  RPVP_ProfMm                   5
004100030716     d  RPVP_DataOra                 14
004200030716     d*---
004300030716     d* DS ridefinizione file piatto d output
004400030716     d*---
004500170125     d fitr4DA1_DS     DS
004600030716     d  tr4ctl                 1      1
004700030716     d  tr4npl                 2      3
004800030716     d  tr4nfv                 4      8
004900030716     d  tr4tim                 9     22
005000030716     d  tr4ms1                23     27
005100030716     d  tr4msx                28     28
005200030716     d  tr4ms2                29     31
005300030716     d  tr4cod                32     36
005400030716     d  tr4020                37     38
005500030716     d  tr4f01                39     39
005600030716     d  tr4pic                40     44
005700030716     d  tr4f02                45     45
005800030716     d  tr4pgr                46     51
005900030716     d  tr4f03                52     52
006000030716     d  tr4pod                53     55
006100030716     d  tr4f04                56     56
006200030716     d  tr4ods                57     57
006300030716     d  tr4f05                58     58
006400030716     d  tr4pad                59     61
006500030716     d  tr4f06                62     62
006600030716     d  tr4dts                63     63
006700030716     d  tr4f07                64     64
006800030716     d  tr4ps1                65     65
006900030716     d  tr4pn1                66     67
007000030716     d  tr4pd1                68     97
007100030716     d  tr4f08                98     98
007200030716     d  tr4ps2                99     99
007300030716     d  tr4pn2               100    101
007400030716     d  tr4pd2               102    130
007500030716     d  tr4xxx               131    131
007600030716     d  tr4f09               132    132
007700030716     d  tr4ps3               133    133
007800030716     d  tr4pn3               134    135
007900030716     d  tr4pd3               136    165
008000030716     d  tr4f10               166    166
008100030716     d  tr4ps4               167    167
008200030716     d  tr4pn4               168    169
008300030716     d  tr4pd4               170    199
008400030716     d  tr4dm1               181    184
008500030716     d  tr4dm2               185    188
008600030716     d  tr4dm3               189    192
008700030716     d  tr4f11               200    200
008800030716     d  tr4pep               201    214
008900030716     d  tr4f12               215    215
009000030716     d  tr4pes               216    216
009100030716     d  tr4f13               217    217
009200030716     d  tr4pup               218    231
009300030716     d  tr4f14               232    232
009400030716     d  tr4pus               233    233
009500030716     d  tr4f15               234    234
009600030716     d  tr4crc               235    236
009700030716     d*---
009800030716     d* Parametri in entrata
009900030716     d*---
010000030716     d  IN_Fil         S              4                                          - filiale
010100030716     d  IN_TFV         S              2                                          - tipo FV
010200030716     d  IN_NFV         S              7                                          - numero FV
010300000000     c*--------------------------------------------------------------------------------------------*
010400000000     c* Main lines
010500000000     c*--------------------------------------------------------------------------------------------*
010600000000     c*
010700030716     c* Leggo tutto il file d input
010800030716     c                   READ      fltr500f
010900030716     c                   DOW       not %eof(fltr500f)
011000091125     c*
011100091125     c* Verifica propedeutica => verifica correttezza presunta formato record dati input
011200091125     c                   if        %len(%trim(tr5dati)) > 83
011300091125     c                   eval      E_TR5KEY  = IN_Fil
011400091125     c                   eval      E_TR5DATI = TR5DATI
011500091125     c                   write     fltr500e
011600091125     c                   delete    fltr5000
011700091125     c                   exsr      sndeml
011800091125     c                   else
011900030716     c*
012000030716     c* Inizializzo il flag del controllo barcode d tipo Bartolini
012100030716     c                   EVAL      flgOK = 'S'
012200030716     c*
012300170125     c* Effettuo trascodifica del FLTR5 e scrivo il FITR4
012400030716     c                   EVAL      RPVP_DS = tr5dati
012500040218     c*
012600040218     c* Verifico subito se è passato il codice tipo applicazione dal file generato sul pc RPVP
012700040220     c                   IF        (RPVP_TipApp =  'C'  OR
012800040220     c                              RPVP_TipApp =  'K'  OR
012900040220     c                              RPVP_TipApp =  'S'  OR
013000040220     c                              RPVP_TipApp =  'A') AND
013100040220     c                              RPVP_ID     <> *blanks
013200170125     c*
013300170125     c* Inizializzo dati di wrk e di output
013400170125     c                   CLEAR                   fitr4
013500170125     c                   CLEAR                   fitr4DA1_DS
013600170125     c*
013700170125     c* Compongo la struttura di DATI
013800030716     c                   EVAL      tr4ctl = %subst(IN_TFV:2:1)
013900030716     c                   EVAL      tr4npl = *zeros
014000030716     c                   EVAL      tr4nfv = %subst(IN_NFV:3:5)
014100030716     c                   EVAL      tr4tim = RPVP_DataOra
014200030716     c                   EVAL      tr4ms1 = *zeros
014300030716     c                   EVAL      tr4msx = *blanks
014400030716     c                   EVAL      tr4ms2 = *blanks
014500030716     c                   EVAL      tr4cod = *zeros
014600030716     c                   EVAL      tr4f01 = *blanks
014700030716     c                   EVAL      tr4pic = *zeros
014800030716     c                   EVAL      tr4f02 = *blanks
014900071011     c                   EVAL      tr4pgr = %subst(RPVP_PesoGr:1:6)
015000030716     c                   EVAL      tr4f03 = *blanks
015100030716     c                   EVAL      tr4pod = '001'
015200030716     c                   EVAL      tr4f04 = *blanks
015300030716     c                   EVAL      tr4f05 = *blanks
015400030716     c                   EVAL      tr4pad = '001'
015500030716     c                   EVAL      tr4f06 = *blanks
015600030716     c                   EVAL      tr4dts = *zeros
015700030716     c                   EVAL      tr4f07 = *blanks
015800030716     c                   EVAL      tr4ps1 = *zeros
015900030716     c                   EVAL      tr4pn1 = *zeros
016000170125     c*
016100030716     c* Verifica se trattasi di barcode Bartolini
016200030716     c* - verifico se è numerico
016300030716     c                   IF        flgOK = 'S'
016400030716     c                   EVAL      PiStr = %trim(RPVP_ID)
016500030716     c                   EXSR      CHKNUM
016600030716     c                   IF        PiInt=*on
016700030716     c                   SETON                                        08
016800030716     c                   ELSE
016900030716     c                   SETOFF                                       08
017000030716     c                   EVAL      flgOK = 'N'
017100030716     c                   ENDIF
017200030716     c                   ENDIF
017300170125     c*
017400030716     c* - verifico se lunghezza è 18
017500030716     c                   IF        flgOK = 'S'
017600030716     c                   IF        %len(%trim(RPVP_ID)) = 18
017700030716     c                   SETON                                        09
017800030716     c                   ELSE
017900030716     c                   SETOFF                                       09
018000030716     c                   EVAL      flgOK = 'N'
018100030716     c                   ENDIF
018200030716     c                   ENDIF
018300170125     c*
018400030716     c* - controllo la LNP
018500030716     c                   IF        flgOK = 'S'
018600030716     c                   EVAL      wPOa = %subst(%trim(RPVP_ID):1:3)
018700030716     c                   MOVE(P)   wPOa          wPOn
018800030716     c     wPOn          CHAIN     azorg01l
018900030716     c                   IF        %found(azorg01l)
019000030716     c                   SETON                                        10
019100030716     c                   ELSE
019200030716     c                   SETOFF                                       10
019300030716     c                   EVAL      flgOK = 'N'
019400030716     c                   ENDIF
019500030716     c                   ENDIF
019600170125     c*
019700030716     c* - controllo la LNA
019800030716     c                   IF        flgOK = 'S'
019900030716     c                   EVAL      wPOa = %subst(%trim(RPVP_ID):4:3)
020000030716     c                   MOVE(P)   wPOa          wPOn
020100030716     c     wPOn          CHAIN     azorg01l
020200030716     c                   IF        %found(azorg01l)
020300030716     c                   SETON                                        11
020400030716     c                   ELSE
020500030716     c                   SETOFF                                       11
020600030716     c                   EVAL      flgOK = 'N'
020700030716     c                   ENDIF
020800030716     c                   ENDIF
020900170125     c*
021000170125     c* ...quindi se segnacolo Bartolini e tipo applicazione K o C:
021100030716     c                   IF        *IN08 = *on AND
021200030716     c                             *IN09 = *on AND
021300030716     c                             *IN10 = *on AND
021400030716     c                             *IN11 = *on AND
021500031212     c                             (RPVP_TipApp = 'K'  or
021600031212     c                              RPVP_TipApp = 'C')
021700030716     c                   EVAL      wBarcode = '#' + %trim(RPVP_ID) + '#'
021800030716     c                   MOVE(P)   wBarcode      tr4pd1
021900030716     c                   ELSE
022000030716     c                   EVAL      wBarcode = '#' + '??????????????????' + '#'
022100030716     c                   MOVE(P)   wBarcode      tr4pd1
022200030716     c                   ENDIF
022300030716     c*
022400030716     c                   EVAL      tr4f08 = *blanks
022500030716     c                   EVAL      tr4ps2 = '6'
022600030716     c                   EVAL      tr4pn2 = '00'
022700170125     c*
022800031205     c* Se segnacolo Bartolini e tipo applicazione K o C:
022900030716     c                   IF        *IN08 = *on AND
023000030716     c                             *IN09 = *on AND
023100030716     c                             *IN10 = *on AND
023200030716     c                             *IN11 = *on AND
023300031212     c                             (RPVP_TipApp = 'K'  or
023400031212     c                              RPVP_TipApp = 'C')
023500030716     c                   EVAL      tr4pd2 = *blanks
023600030716     c                   ELSE
023700030722     c                   EVAL      tr4pd2 = *all'?'
023800030722     c                   EVAL      %subst(tr4pd2:1:1) = '#'
023900030722     c                   EVAL      %subst(tr4pd2:(%len(tr4pd2)-
024000030722     c                             %len(%trim(RPVP_ID))+1):%len(%trim(RPVP_ID)))
024100030722     c                             = %trim(RPVP_ID)
024200030716     c                   ENDIF
024300030716     c*
024400030716     c                   EVAL      tr4f09 = *blanks
024500030716     c                   EVAL      tr4ps3 = *zeros
024600030716     c                   EVAL      tr4pn3 = *zeros
024700071127     c***                EVAL      tr4pd3 = *zeros
024800071127     c                   EVAL      tr4pd3 = %subst(RPVP_PesoGr:1:6)
024900030716     c                   EVAL      tr4f10 = *blanks
025000030716     c                   EVAL      tr4ps4 = *zeros
025100030716     c                   EVAL      tr4pn4 = *zeros
025200170125     c*
025300170125     c* Eseguo alcune considerazioni x la valorizzazione (troncata) delle misure
025400030718     c*
025500030718     c* Dal volume reperisco il lato cubico approssimato
025600030721     c                   MOVE(P)   RPVP_VolCm3   wVolume
025700030721     c                   EVAL      wLatoCub = wVolume**(1/3)
025800071011     c                   EVAL      wLatoCub = wLatoCub * 10
025900030721     c                   EVAL      wLatoCubInt = %inth(wLatoCub)
026000031215     c                   MOVE(P)   wLatoCubInt   wLatoCubIntA
026100030721     c* - lunghezza
026200030721     c                   MOVE(P)   wLatoCubIntA  tr4dm1
026300030716     c* - altezza
026400030721     c                   MOVE(P)   wLatoCubIntA  tr4dm2
026500030716     c* - profondità
026600030721     c                   MOVE(P)   wLatoCubIntA  tr4dm3
026700030716     c*
026800030716     c                   EVAL      tr4f11 = *blanks
026900170125     c*
027000031205     c* Imposto baia entrata e tipo record
027100031205     c                   SELECT
027200031205     C                   when      RPVP_TipApp = 'C'
027300031205     c                   MOVE(P)   'RPVP_C'      tr4pep
027400031205     c                   EVAL      tr4020 = '2C'
027500031205     c                   when      RPVP_TipApp = 'K'
027600030722     c                   MOVE(P)   'RPVP_K'      tr4pep
027700031205     c                   EVAL      tr4020 = '2K'
027800031205     c                   when      RPVP_TipApp = 'S'
027900030722     c                   MOVE(P)   'RPVP_S'      tr4pep
028000031205     c                   EVAL      tr4020 = '22'
028100031205     c                   when      RPVP_TipApp = 'A'
028200031205     c                   MOVE(P)   'RPVP_A'      tr4pep
028300031205     c                   EVAL      tr4020 = '22'
028400031205     c                   other
028500031205     c                   MOVE(P)   'RPVP_ERR'    tr4pep
028600031205     c                   EVAL      tr4020 = '22'
028700031205     c                   ENDSL
028800030722     c*
028900030716     c                   EVAL      tr4f12 = *blanks
029000030716     c                   EVAL      tr4pes = '1'
029100030716     c                   EVAL      tr4f13 = *blanks
029200030716     c                   EVAL      tr4pup = *blanks
029300030716     c                   EVAL      tr4f14 = *blanks
029400170125     c*
029500031205     c* Se segnacolo Bartolini e tipo applicazione K o C:
029600030716     c                   IF        *IN08 = *on AND
029700030716     c                             *IN09 = *on AND
029800030716     c                             *IN10 = *on AND
029900030725     c                             *IN11 = *on AND
030000031212     c                             (RPVP_TipApp = 'K'  or
030100031212     c                              RPVP_TipApp = 'C')
030200030716     c                   EVAL      tr4pus = '1'
030300030716     c                   ELSE
030400030716     c                   EVAL      tr4pus = '0'
030500030716     c                   ENDIF
030600030716     c*
030700030725     c                   IF        tr4pus = '0'
030800030725     c                   EVAL      tr4ods = '3'
030900030725     c                   ELSE
031000030725     c                   EVAL      tr4ods = '1'
031100030725     c                   ENDIF
031200030725     c*
031300030716     c                   EVAL      tr4f15 = *blanks
031400030716     c                   EVAL      tr4crc = *blanks
031500030716     c*
031600170125     c* Al termine della trascodifica scrivo il FITR4 e cancello il record d input dal FLTR5
031700170125     c                   eval      tr4fil    = %subst(IN_Fil:2:3)
031800170125     c                   eval      tr4pc     = *blanks
031900170125     c                   eval      tr4da1    = fitr4DA1_DS
032000170125     c                   eval      tr4datora = %editc(%dec(%date():*ISO):'X')+
032100170125     c                                         %editc(%dec(%time():*ISO):'X')
032200170125     c                   write     fitr4
032300030716     c                   delete    fltr5000
032400040220     c                   ENDIF
032500091125     c*
032600091125     c                   endif
032700030716     c*
032800030716     c* Proseguo con la lettura
032900030716     c                   READ      fltr500f
033000030716     c                   ENDDO
033100000000     c*
033200030716     c                   SETON                                        LR
033300170125
033400170125
033500170125
033600030716     c*--------------------------------------------------------------------------------------------*
033700030716     c*  CONTROLLO NUMERICITA'
033800030716     c*--------------------------------------------------------------------------------------------*
033900030716     c     CHKNUM        BEGSR
034000030716     c*
034100030716     c                   IF        PiDecChr = *blanks
034200030716     c                   EVAL      PiDecChr = ','
034300030716     c                   ENDIF
034400030716     c*
034500030716     c                   CALL(e)   'ISNUMERIC'
034600030716     c                   PARM                    PiStr            30
034700030716     c                   PARM                    PiDecChr          1
034800030716     c                   PARM      *ZEROS        PiVal            30 9
034900030716     c                   PARM      '0'           PiInt             1
035000030716     c                   PARM      '0'           PiNum             1
035100030716     c                   IF        %error
035200030716     c                   EVAL      PiNum=*off
035300030716     c                   ENDIF
035400030716     c*
035500030716     c                   ENDSR
035600170125
035700170125
035800091125
035900091125     c*--------------------------------------------------------------------------------------------*
036000120302     c* /TITLE Compongo il testo e spedisco una @-mail
036100091125     c*--------------------------------------------------------------------------------------------*
036200091125     c     sndeml        begsr
036300091125     c*
036400091125     c* Inizializzo variabili
036500091125     c                   movel     *blanks       wrkEml          253
036600091125     c                   movel     *blanks       wrkEmlMsg      5000
036700091125     c                   movel     *blanks       wrkEmlOgg        44
036800120302     c* Valorizzo i campi della @-mail
036900120302     c                   eval      wrkEml='cedalert@brt.it '
037000091125     c                   eval      wrkEmlOgg='SPUNTE DA RPVP - ATTENZIONE !!!'
037100091125     c                   EVAL      wrkEmlMsg = 'Presenti errori formali nei '  +
037200091125     c                             'record spunta da RPVP per la filiale '     +
037300091125     c                             %subst(IN_Fil:2:3) + '.'              +':/N'+
037400091125     c                                                                    ':/N'+
037500091125     c                             'Verificare i dati errati nel file FLTR5ERR'+
037600091125     c                             ' con chiave: ' + IN_Fil + '.'
037700091125     c*
037800091125     c                   call(e)   'TIS701C'
037900091125     c                   parm                    wrkEml
038000091125     c                   parm                    wrkEmlOgg
038100091125     c                   parm                    wrkEmlMsg
038200091125     c*
038300091125     c                   endsr
038400170125
038500170125
038600091125
038700000000     c*--------------------------------------------------------------------------------------------*
038800030716     c* Esegue operazioni iniziali
038900030716     c*--------------------------------------------------------------------------------------------*
039000030716     c     *inzsr        BEGSR
039100030716     c*
039200030716     c* Ricezione parametri in entrata
039300030716     c     *ENTRY        PLIST
039400030716     c                   PARM                    IN_Fil
039500030716     c                   PARM                    IN_TFV
039600030716     c                   PARM                    IN_NFV
039700030716     c*
039800030716     c                   ENDSR
