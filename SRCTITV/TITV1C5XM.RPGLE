000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600071122     FTITAS30C  IF   E           K DISK
000700071122     F                                     rename(titas000:titas0)
000800071122     F                                     rename(titas010:titas1)
000900071122     F                                     rename(titasP00:titasP)
001000071121
001100071121     D*------------
001200071121     D* DECODIFICHE COSTANTI
001300071121     D*------------
001400080110     D DECINT          S            100    DIM(35) CTDATA PERRCD(1)
001500080110     D DECFIN          S            100    DIM(01) CTDATA PERRCD(1)
001600080114
001700080114
001800080114     D*------------
001900080114     D* DS D WRK
002000080114     D*------------
002100080924     D FNVAC00T      E DS
002200071121
002300071121
002400071121     D*------------------
002500071121     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600071121     D*------------------
002700071121     D WLBDA8          DS                  INZ
002800071121     D  G08DAT                 1      8  0
002900071121     D  G08INV                 9     16  0
003000071121     D  G08ERR                17     17
003100071121     D  G08TGI                18     22  0
003200071121
003300071121     D*------------
003400071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003500071121     D*------------
003600080110     D iWrSh           s            100    inz('<Worksheet ss:Name="PAG§§§§">')
003700080110     D fWrSh           s            100    inz('</Worksheet>')
003800080110     D
003900080110     D iTbl            s            100    inz('<Table ss:border="1">')
004000080110     D fTbl            s            100    inz('</Table>')
004100080110     D
004200080114     D iCol1           s            100    inz('<Cell ss:StyleID="')
004300080110     D iCol2           s            100    inz('"><Data ss:Type="String">')
004400080111     D iCol3           s            100    inz('<![CDATA[')
004500080111     D fCol            s            100    inz(']]></Data></Cell>')
004600080110     D
004700080110     D iRow            s            100    inz('<Row>')
004800080110     D fRow            s            100    inz('</Row>')
004900080110     D
005000080110     D ssInt           s            100    inz('ColYellGriglia')
005100080110     D ssStd           s            100    inz('Griglia')
005200080114
005300080114
005400080114     D*------------
005500080114     D* COSTANTI X UTILITY CONTROLLO CARATTERI VALIDI
005600080114     D*------------
005700080114     D CharOk          C                   const('QWERTYUIOPASDFGHJKLZXCVBNM-
005800080111     D                                            qwertyuiopasdfghjklzxcvbnm-
005900080114     D                                            1234567890.,;:-_/\()!£$%=?-
006000080114     D                                            +*§@')
006100080114     D TxtInOut        S           2048
006200080114     D ElencoChar      S            256
006300080114     D TipoElenco      S              1
006400080114     D CharSost        S              1
006500080114     D UpperCase       S              1
006600080114     D ChkNull         S              1
006700080114     D CharNull        S              1
006800080114     D EsitoBon        S              1
006900071121
007000071121
007100071121     D*------------
007200071121     D* VARIABILI D WRK
007300071121     D*------------
007400030924     D prmppt          S             50
007500030924     D prmesito        S              1
007600030924     D wrkesito        S                   like(prmesito)
007700071121     D wConsOkSiNo     S              1    inz(*blanks)
007800071121     D wConsOkSiNoDes  S            300    inz(*blanks)
007900071122     D wBufferOut      s          10000    varying inz
008000071122     D wIndirizzo      s             35    inz(*blanks)
008100071122     D wLocalita       s             35    inz(*blanks)
008200071122     D wDSP            s                   like(VACDCM) inz(*zeros)
008300071122     D wDCM            s                   like(VACDCM) inz(*zeros)
008400071122     D wHMC            s                   like(VACHMC) inz(*zeros)
008500080110     D wCntRec         s              7  0 inz
008600080110     D wCntWrSh        s              4  0 inz(1)
008700080114     D wiCol           s           1000    varying inz
008800080114     D wfCol           s           1000    varying inz
008900080110     D wCntWrShAlfa    s              4    inz
009000030924
009100030325
009200030924     C* MAIN/
009300071121     C*
009400071121     C* Verifico se già presenti dati nel file d output
009500071121     C                   EXSR      chkRECFILMBR
009600071121     C*
009700071121     C* Effettuo elaborazione
009800071121     C                   EXSR      traduci
009900071121     C*
010000921023     C                   SETON                                        LR
010100991027
010200071121
010300071121
010400071121     C     chkRECFILMBR  BEGSR
010500071121     C*
010600071121     C                   READ      TIVAWWWT                               55
010700071121     C*
010800071121     C                   ENDSR
010900071121
011000071121
011100030924
011200991027     C     traduci       BEGSR
011300071121     C*
011400080110     C* Head iniziali della struttura XML
011500080111     C                   EXSR      exeWrBkStr
011600071121     C*
011700071121     C* Inizializzazone formato e variabili x output
011800071121     C                   clear                   TIVAW000
011900071121     C                   clear                   wBufferOut
012000991027     C*
012100080924     C                   READ      TIVGDTMP
012200060119     C*
012300080924     C                   DOW       not %eof(TIVGDTMP)
012400080924     C*
012500080924     C                   eval      fnvac00t = vgdDTA
012600080114     C*
012700080114     C* Controllo subito i caratteri nn validi
012800080114     C                   EXSR      exebon
012900080110     C*
013000080110     C                   clear                   wConsOkSiNo
013100080110     C                   clear                   wConsOkSiNoDes
013200080110     C                   clear                   TIVAW000
013300080110     C                   clear                   wBufferOut
013400080110     C                   clear                   wIndirizzo
013500080110     C                   clear                   wLocalita
013600080110     C*
013700080110     C* Operazioni x primo record
013800080110     C                   ADD       1             wCntRec
013900080110     C                   IF        wCntRec=65500
014000080110     C                   Z-ADD     1             wCntRec
014100080110     C                   ADD       1             wCntWrSh
014200080110     C                   ENDIF
014300080110     C*
014400080110     C                   IF        wCntRec=1
014500080110     C                   IF        wCntWrSh>1
014600080110     C                   EXSR      exeWrShEnd
014700080110     C                   ENDIF
014800080110     C                   EXSR      exeWrShStr
014900080110     C                   EXSR      exeIntCol
015000080110     C                   ENDIF
015100071122     C*
015200071122     C* Compongo subito la data spedizine
015300071122     C                   eval      wDSP = vacAAS*10000+vacMGS
015400040719     C*
015500071121     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
015600060208     C                   IF        VACDCM > *zeros AND
015700071121     C                             (VACCCA = *blanks OR VACCCA = '1')
015800071121     C                   EVAL      wConsOkSiNo    = 'S'
015900071121     C                   ELSE
016000071121     C                   EVAL      wConsOkSiNo    = 'N'
016100071121     C*
016200071121     C* A questo punto stabilisco la descrizione
016300071121     C                   IF        vacCCA = '1'
016400071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione dirottata '+
016500071121     C                                              'ad altra filiale.'
016600071121     C                   ENDIF
016700071121     C                   IF        vacCCA = '2'
016800071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione resa al '+
016900071121     C                                              'mittente.'
017000071121     C                   ENDIF
017100071121     C                   IF        vacCCA = '3'
017200071121     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce.'
017300071121     C                   ENDIF
017400071121     C                   IF        vacCCA = '4'
017500071121     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso.'
017600071121     C                   ENDIF
017700071121     C                   IF        vacCCA = '5'
017800071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
017900071121     C                                              'con pratica anomalia.'
018000071121     C                   ENDIF
018100071121     C                   IF        vacCCA = '6'
018200071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
018300071121     C                                              'con avaria resa al '+
018400071121     C                                              'mittente.'
018500071121     C                   ENDIF
018600071121     C                   IF        vacCCA = 'A'
018700071121     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica '+
018800071121     C                                              'anomalia.'
018900071121     C                   ENDIF
019000071121     C                   IF        vacCCA = 'C'
019100071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione in consegna.'
019200071121     C                   ENDIF
019300071121     C                   IF        vacCCA = 'A'
019400071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata '+
019500071121     C                                              'dopo errata imputazione '+
019600071121     C                                              'di consegna.'
019700071121     C                   ENDIF
019800071121     C                   IF        vacCCA = 'S'
019900071121     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta.'
020000071121     C                   ENDIF
020100071121     C*
020200071121     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
020300071121     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso.'
020400071121     C                   ENDIF
020500071121     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
020600071121     C                   EVAL      wConsOkSiNoDes = 'Aperta giacienza.'
020700071121     C                   ENDIF
020800071121     C*
020900071121     C                   ENDIF
021000071122     C*
021100071122     C* Aggancio la bolla relativa x il completamento dei dati
021200071122     C     KEYtas30      chain     titas30c
021300071122     C                   if        %found(titas30c)
021400071122     C*
021500071122     C* reperisco i dati del destinatario nn già presenti sul FNVAC
021600071122     C                   eval      wIndirizzo = tasIND
021700071122     C                   eval      wLocalita  = tasLOD
021800071122     C                   endif
021900071121     C*
022000071121     C* Compondo la stringa dati output relativa al record corrente
022100071121     C                   EXSR      wrifile
022200040719     C*
022300040719     C* In ogni caso storicizzo e cancello l'input sempre
022400080924     C                   DELETE    TIVGD000
022500040719     C*
022600040719     C* Proseguo con la lettura dell'input
022700080924     C                   READ      TIVGDTMP
022800060119     C*
022900030325     C                   ENDDO
023000071121     C*
023100071121     C* Head finali della struttura HTML
023200080110     C                   EXSR      exeWrShEnd
023300080111     C                   EXSR      exeWrBkEnd
023400991027     C*
023500030325     C                   EVAL      wrkesito = '0'
023600991027     C*
023700910830     C                   ENDSR
023800080110
023900080110
024000080110
024100080110     C     exeIntCol     BEGSR
024200080110     C*
024300080110     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
024400080110     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
024500080111     C                   EVAL      wiCol=%trim(iCol1)+%trim(ssInt)+%trim(iCol2)+
024600080111     C                                   %trim(iCol3)
024700080110     C                   EVAL      wfCol=%trim(fCol)
024800080114     C                   EVAL      VAWDTA =
024900080114     C                             %trim(iRow)
025000080114     C                   WRITE     TIVAW000
025100080114     C                   EVAL      VAWDTA =
025200080114     C                             wiCol+'N.BOLLA'+wfCol
025300080114     C                   WRITE     TIVAW000
025400080114     C                   EVAL      VAWDTA =
025500080114     C                             wiCol+'N.SPED'+wfCol
025600080114     C                   WRITE     TIVAW000
025700080114     C                   EVAL      VAWDTA =
025800080114     C                             wiCol+'DATA SPED'+wfCol
025900080114     C                   WRITE     TIVAW000
026000080114     C                   EVAL      VAWDTA =
026100080114     C                             wiCol+'DATA CONS'+wfCol
026200080114     C                   WRITE     TIVAW000
026300080114     C                   EVAL      VAWDTA =
026400080114     C                             wiCol+'STATO'+wfCol
026500080114     C                   WRITE     TIVAW000
026600080114     C                   EVAL      VAWDTA =
026700080114     C                             wiCol+'MOTIVO MANCATA CONS'+wfCol
026800080114     C                   WRITE     TIVAW000
026900080114     C                   EVAL      VAWDTA =
027000080114     C                             wiCol+'ORA CONS'+wfCol
027100080114     C                   WRITE     TIVAW000
027200080114     C                   EVAL      VAWDTA =
027300080114     C                             wiCol+'DESTINATARIO'+wfCol
027400080114     C                   WRITE     TIVAW000
027500080114     C                   EVAL      VAWDTA =
027600080114     C                             wiCol+'INDIRIZZO'+wfCol
027700080114     C                   WRITE     TIVAW000
027800080114     C                   EVAL      VAWDTA =
027900080114     C                             wiCol+'CITTA'+wfCol
028000080114     C                   WRITE     TIVAW000
028100080114     C                   EVAL      VAWDTA =
028200080114     C                             wiCol+'PROV'+wfCol
028300080114     C                   WRITE     TIVAW000
028400080114     C                   EVAL      VAWDTA =
028500080114     C                              %trim(fRow)
028600080114     C                   WRITE     TIVAW000
028700080110     C                   ENDIF
028800080110     C*
028900080110     C                   ENDSR
029000080110
029100080110
029200080110
029300080110     C     exeWrShStr    BEGSR
029400080110     C*
029500080110     C                   EVAL      wCntWrShAlfa=%trim(%editw(wCntWrSh:'0    '))
029600080114     C                   EVAL      VAWDTA  =  %replace(wCntWrShAlfa:iWrSh:
029700080110     C                                   %scan('§§§§':iWrSh))
029800080111     C                   WRITE     TIVAW000
029900080114     C*
030000080114     C                   EVAL      VAWDTA  =  %trim(iTbl)
030100080114     C                   WRITE     TIVAW000
030200080110     C*
030300080110     C                   ENDSR
030400080110
030500080110
030600080110
030700080110     C     exeWrShEnd    BEGSR
030800080114     C*
030900080114     C                   EVAL      VAWDTA  =  %trim(fTbl)
031000080114     C                   WRITE     TIVAW000
031100080110     C*
031200080110     C                   EVAL      VAWDTA = fWrSh
031300080111     C                   WRITE     TIVAW000
031400080110     C*
031500080110     C                   ENDSR
031600080111
031700080111
031800080111
031900080111     C     exeWrBkStr    BEGSR
032000080111     C*
032100080111     C                   EVAL      VAWDTA = %trim(DECINT(01))
032200080111     C                   WRITE     TIVAW000
032300080111     C                   EVAL      VAWDTA = %trim(DECINT(02))
032400080111     C                   WRITE     TIVAW000
032500080111     C                   EVAL      VAWDTA = %trim(DECINT(03))
032600080111     C                   WRITE     TIVAW000
032700080111     C                   EVAL      VAWDTA = %trim(DECINT(04))
032800080111     C                   WRITE     TIVAW000
032900080111     C                   EVAL      VAWDTA = %trim(DECINT(05))
033000080111     C                   WRITE     TIVAW000
033100080111     C                   EVAL      VAWDTA = %trim(DECINT(06))
033200080111     C                   WRITE     TIVAW000
033300080111     C                   EVAL      VAWDTA = %trim(DECINT(07))
033400080111     C                   WRITE     TIVAW000
033500080111     C                   EVAL      VAWDTA = %trim(DECINT(08))
033600080111     C                   WRITE     TIVAW000
033700080111     C                   EVAL      VAWDTA = %trim(DECINT(09))
033800080111     C                   WRITE     TIVAW000
033900080111     C                   EVAL      VAWDTA = %trim(DECINT(10))
034000080111     C                   WRITE     TIVAW000
034100080111     C                   EVAL      VAWDTA = %trim(DECINT(11))
034200080111     C                   WRITE     TIVAW000
034300080111     C                   EVAL      VAWDTA = %trim(DECINT(12))
034400080111     C                   WRITE     TIVAW000
034500080111     C                   EVAL      VAWDTA = %trim(DECINT(13))
034600080111     C                   WRITE     TIVAW000
034700080111     C                   EVAL      VAWDTA = %trim(DECINT(14))
034800080111     C                   WRITE     TIVAW000
034900080111     C                   EVAL      VAWDTA = %trim(DECINT(15))
035000080111     C                   WRITE     TIVAW000
035100080111     C                   EVAL      VAWDTA = %trim(DECINT(16))
035200080111     C                   WRITE     TIVAW000
035300080111     C                   EVAL      VAWDTA = %trim(DECINT(17))
035400080111     C                   WRITE     TIVAW000
035500080111     C                   EVAL      VAWDTA = %trim(DECINT(18))
035600080111     C                   WRITE     TIVAW000
035700080111     C                   EVAL      VAWDTA = %trim(DECINT(19))
035800080111     C                   WRITE     TIVAW000
035900080111     C                   EVAL      VAWDTA = %trim(DECINT(20))
036000080111     C                   WRITE     TIVAW000
036100080111     C                   EVAL      VAWDTA = %trim(DECINT(21))
036200080111     C                   WRITE     TIVAW000
036300080111     C                   EVAL      VAWDTA = %trim(DECINT(22))
036400080111     C                   WRITE     TIVAW000
036500080111     C                   EVAL      VAWDTA = %trim(DECINT(23))
036600080111     C                   WRITE     TIVAW000
036700080111     C                   EVAL      VAWDTA = %trim(DECINT(24))
036800080111     C                   WRITE     TIVAW000
036900080111     C                   EVAL      VAWDTA = %trim(DECINT(25))
037000080111     C                   WRITE     TIVAW000
037100080111     C                   EVAL      VAWDTA = %trim(DECINT(26))
037200080111     C                   WRITE     TIVAW000
037300080111     C                   EVAL      VAWDTA = %trim(DECINT(27))
037400080111     C                   WRITE     TIVAW000
037500080111     C                   EVAL      VAWDTA = %trim(DECINT(28))
037600080111     C                   WRITE     TIVAW000
037700080111     C                   EVAL      VAWDTA = %trim(DECINT(29))
037800080111     C                   WRITE     TIVAW000
037900080111     C                   EVAL      VAWDTA = %trim(DECINT(30))
038000080111     C                   WRITE     TIVAW000
038100080111     C                   EVAL      VAWDTA = %trim(DECINT(31))
038200080111     C                   WRITE     TIVAW000
038300080111     C                   EVAL      VAWDTA = %trim(DECINT(32))
038400080111     C                   WRITE     TIVAW000
038500080111     C                   EVAL      VAWDTA = %trim(DECINT(33))
038600080111     C                   WRITE     TIVAW000
038700080111     C                   EVAL      VAWDTA = %trim(DECINT(34))
038800080111     C                   WRITE     TIVAW000
038900080111     C                   EVAL      VAWDTA = %trim(DECINT(35))
039000080111     C                   WRITE     TIVAW000
039100080111     C*
039200080111     C                   ENDSR
039300080111
039400080111
039500080114
039600080111
039700080111     C     exeWrBkEnd    BEGSR
039800080111     C*
039900080111     C                   EVAL      VAWDTA = %trim(DECFIN(01))
040000080111     C                   WRITE     TIVAW000
040100080111     C*
040200080111     C                   ENDSR
040300080114
040400080114
040500071121
040600071121
040700080114     C     wrifile       BEGSR
040800071122     C*
040900071122     C* Se spedizione nn consegnata OK => azzero cmq la data consegna (ovvero nn indico
041000071122     C* la data stato)
041100071122     C                   IF        wConsOkSiNo = 'S'
041200071122     C                   EVAL      wDCM = VACDCM
041300071122     C                   EVAL      wHMC = VACHMC
041400071122     C                   ELSE
041500071122     C                   EVAL      wDCM = *zeros
041600071122     C                   EVAL      wHMC = *zeros
041700071122     C                   ENDIF
041800071121     C*
041900080110     C                   CLEAR                   wBufferOut
042000080111     C                   EVAL      wiCol=%trim(iCol1)+%trim(ssStd)+%trim(iCol2)+
042100080111     C                                   %trim(iCol3)
042200080110     C                   EVAL      wfCol=%trim(fCol)
042300080114     C                   EVAL      VAWDTA = %trim(iRow)
042400080114     C                   WRITE     TIVAW000
042500080114     C                   EVAL      VAWDTA =
042600080114     C                             wiCol+%trim(%editc(VACRMN:'Q'))+wfCol
042700080114     C                   WRITE     TIVAW000
042800080114     C                   EVAL      VAWDTA =
042900080114     C                             wiCol+%trim(%editw(VACAAS:'0    '))+
043000080114     C                                   %trim(%editw(VACLNP:'0   '))+
043100080114     C                                   %trim(%editw(VACNRS:'0  '))+
043200080114     C                                   %trim(%editw(VACNSP:'0       '))+wfCol
043300080114     C                   WRITE     TIVAW000
043400080114     C                   EVAL      VAWDTA =
043500080114     C                             wiCol+%trim(%editw(wDSP:'    /  /  '))+wfCol
043600080114     C                   WRITE     TIVAW000
043700080114     C                   EVAL      VAWDTA =
043800080114     C                             wiCol+%trim(%editw(wDCM:'    /  /  '))+wfCol
043900080114     C                   WRITE     TIVAW000
044000080114     C                   EVAL      VAWDTA =
044100080114     C                                  wiCol+%trim(wConsOkSiNo)+wfCol
044200080114     C                   WRITE     TIVAW000
044300080114     C                   EVAL      VAWDTA =
044400080114     C                                  wiCol+%trim(wConsOkSiNoDes)+wfCol
044500080114     C                   WRITE     TIVAW000
044600080114     C                   EVAL      VAWDTA =
044700080114     C                             wiCol+%trim(%editw(wHMC:'  :  '))+wfCol
044800080114     C                   WRITE     TIVAW000
044900080114     C                   EVAL      VAWDTA =
045000080114     C                                  wiCol+%trim(VACRSD)+wfCol
045100080114     C                   WRITE     TIVAW000
045200080114     C                   EVAL      VAWDTA =
045300080114     C                                  wiCol+%trim(wIndirizzo)+wfCol
045400080114     C                   WRITE     TIVAW000
045500080114     C                   EVAL      VAWDTA =
045600080114     C                                  wiCol+%trim(wLocalita)+wfCol
045700080114     C                   WRITE     TIVAW000
045800080114     C                   EVAL      VAWDTA =
045900080110     C                                  wiCol+%trim(VACPRD)+wfCol
046000080114     C                   WRITE     TIVAW000
046100080114     C                   EVAL      VAWDTA =
046200080114     C                              %trim(fRow)
046300080114     C                   WRITE     TIVAW000
046400071121     C*
046500071121     C                   ENDSR
046600080114
046700080114
046800080114
046900080114
047000080114      /TITLE "Bonifica" dati bolla da caratteri nn voluti (tramite XCHKCHAR)
047100080114     C     exebon        BEGSR
047200080114     C*
047300080114     C* Imposto i parametri x il *pgm d "bonifica"
047400080114     C                   EVAL      ElencoChar = CharOk
047500080114     C                   EVAL      TipoElenco = *blanks
047600080114     C                   EVAL      CharSost   = *blanks
047700080114     C                   EVAL      UpperCase  = '1'
047800080114     C                   EVAL      ChkNull    = '1'
047900080114     C                   EVAL      CharNull   = *blanks
048000080114     C*
048100080114     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
048200080924     C                   EVAL      TxtInOut   = FNVAC00T
048300080114     C                   EVAL      EsitoBon   = *blanks
048400080114     C*
048500080114     C                   CALL      'XCHKCHAR'
048600080114     C                   PARM                    TxtInOut
048700080114     C                   PARM                    ElencoChar
048800080114     C                   PARM                    TipoElenco
048900080114     C                   PARM                    CharSost
049000080114     C                   PARM                    UpperCase
049100080114     C                   PARM                    ChkNull
049200080114     C                   PARM                    CharNull
049300080114     C                   PARM                    EsitoBon
049400080114     C*
049500080114     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
049600080114     C                   IF        EsitoBon  = '1'
049700080924     C                   EVAL      FNVAC00T = TxtInOut
049800080114     C                   ENDIF
049900080114     C*
050000080114     C                   ENDSR
050100071121
050200071121
050300071121
050400991027
050500991027      /TITLE Operazioni iniziali.
050600991027     C     *inzsr        BEGSR
050700991027     C*
050800991027     C     *ENTRY        PLIST
050900991027     C                   parm                    prmppt
051000991027     C     wrkesito      parm      wrkesito      prmesito
051100071122     C*
051200071122     C* TITAS30C - Completa
051300071122     C     KEYtas30      KLIST
051400071122     C                   KFLD                    vacAAS
051500071122     C                   KFLD                    vacLNP
051600071122     C                   KFLD                    vacNRS
051700071122     C                   KFLD                    vacNSP
051800991027     C*
051900991027     C                   ENDSR
052000071121     C*
052100080110** DECINT - COSTANTI
052200080111<?xml version="1.0"?>
052300080110<?mso-application progid="Excel.Sheet"?>
052400080110<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
052500080110 xmlns:o="urn:schemas-microsoft-com:office:office"
052600080110 xmlns:x="urn:schemas-microsoft-com:office:excel"
052700080110 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
052800080110 xmlns:html="http://www.w3.org/TR/REC-html40">
052900080110
053000080110 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
053100080110 </DocumentProperties>
053200080110
053300080110 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
053400080110 </ExcelWorkbook>
053500080110
053600080110 <Styles>
053700080110  <Style ss:ID="ColYellGriglia">
053800080110   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
053900080110   <Borders>
054000080110    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
054100080110    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
054200080110    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
054300080110    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
054400080110   </Borders>
054500080110  </Style>
054600080110
054700080110  <Style ss:ID="Griglia">
054800080110   <Borders>
054900080110    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
055000080110    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
055100080110    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
055200080110    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
055300080110   </Borders>
055400080110  </Style>
055500080110 </Styles>
055600080110
055700080110** DECFIN - COSTANTI
055800080110</Workbook>
