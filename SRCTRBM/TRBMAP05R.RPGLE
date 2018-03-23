000100030401     H*--------------------------------------------------------------------------------------------*
000200030401     H DECEDIT('0,') DATEDIT(*DMY.)
000300150618     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400030401     H*--------------------------------------------------------------------------------------------*
000500160112     FTNTBE01L  IF   E           K DISK
000600160127     FWFMAF050F O    E             DISK
000700030404     D*---
000800030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
000900030404     D*---
001000091211     D TRBMFCDS      e ds
001100160112     D*---
001200160112     D* DS ridefinizione tabelle di procedura
001300160112     D*---
001400160112     D DAOA          e ds
001500160115     D*---
001600160115     D* Variabili di wrk
001700160115     D*---
001800160203     D SUBFLDR_ARCHI   s                   like(SUBFLDR) inz
001900160203     D KTBECOD         s                   like(TBECOD) inz
002000160203     D KTBEKE1         s                   like(TBEKE1) inz
002100160203     D KTBEKE2         s                   like(TBEKE2) inz
002200171011     D wFLGREPLACE     s                   like(FLGREPLACE) inz
002300160112     D*---
002400160127     D* Program Status DS
002500160127     D*---
002600160127     D psds           sds
002700160127     D  jobname              244    253
002800160127     D  jobuser              254    263
002900160127     D  jobnumber            264    269
003000160127
003100160127
003200070607     C*
003300070607     C* Ricevimento parametri
003400090513     C     *ENTRY        PLIST
003500160129     C                   PARM                    APLKEY           15
003600090514     C                   PARM                    IFSNAM           10
003700090513     C                   PARM                    FILNAM           10
003800090513     C                   PARM                    JOBNAM           10
003900090513     C                   PARM                    USRNAM           10
004000090513     C                   PARM                    JOBNUM            6
004100090513     C                   PARM                    NUMFIL            6
004200160112     C                   PARM                    FLGDATAORA        1
004300170605     C                   PARM                    FILETYPE          3
004400160112     C                   PARM                    FILEOUT         255
004500090514     C                   PARM                    SUBFLDR          50
004600160115     C                   PARM                    FLGSUBFLDR        1
004700160120     C                   PARM                    FLGREPLACE        1
004800160127     C                   PARM                    FLGLOG            1
004900170605     C                   PARM                    FLGDELSPL         1
005000160119     C                   PARM                    ESITO             1
005100160112     C*
005200160115     C* Reperisco la DATA e ORA corrente
005300160112     C                   TIME                    wn14             14 0
005400160112     C                   MOVEL     wn14          ORACOR            6 0          *ORA
005500160112     C                   Z-ADD     *zeros        DATCOR            8 0          *DATA CORRENTE AA/M/
005600160112     C                   EVAL      DATCOR = %dec(%date() : *ISO)
005700160129     C*
005800160115     C* Gestisco se richiesto automatismo per aggiunta automatica timestamp al nome del file
005900160119     C                   MOVEL     *blanks       wDATAORA         15
006000160115     C                   IF        FLGDATAORA = 'S'
006100160112     C                   EVAL      wDATAORA = '_' + %trim(%editc(DATCOR:'X')) +
006200160112     C                                              %trim(%editc(ORACOR:'X'))
006300160112     C                   ENDIF
006400170605     C*
006500170605     C* Gestisco valori possibili per parametro FILETYPE
006600170605     C                   SELECT
006700170605     C                   WHEN      FILETYPE = 'TXT' or FILETYPE = 'txt'
006800170605     C                   EVAL      FILETYPE = 'TXT'
006900170605     C                   WHEN      FILETYPE = 'PDF' or FILETYPE = 'pdf'
007000170605     C                   EVAL      FILETYPE = 'PDF'
007100170605     C                   OTHER
007200170605     C                   EVAL      FILETYPE = 'TXT'
007300170605     C                   ENDSL
007400170605     C*
007500170605     C* Gestisco valori possibili per parametro FLGDELSPL
007600170605     C                   SELECT
007700170605     C                   WHEN      FLGDELSPL = 'N' or FLGDELSPL = 'n'
007800170605     C                   EVAL      FLGDELSPL = 'N'
007900170605     C                   WHEN      FLGDELSPL = 'S' or FLGDELSPL = 's'
008000170605     C                   EVAL      FLGDELSPL = 'S'
008100170605     C                   WHEN      FLGDELSPL = 'Y' or FLGDELSPL = 'y'
008200170605     C                   EVAL      FLGDELSPL = 'S'
008300170605     C                   OTHER
008400170605     C                   EVAL      FLGDELSPL = 'N'
008500170605     C                   ENDSL
008600160112     C*
008700160112     C* Definizione chiavi di lettura
008800160112     C*
008900160112     C* TNTBE01L - Parziale
009000160112     C     KEYTBE01_P    KLIST
009100160203     C                   KFLD                    KTBECOD
009200160203     C                   KFLD                    KTBEKE1
009300160203     C                   KFLD                    KTBEKE2
009400160112     C*
009500160112     C* Reperisco la descrizione della fase da tabella
009600160203     C                   MOVEL(p)  'AOA'         KTBECOD
009700160203     C                   MOVEL(p)  APLKEY        KTBEKE1
009800160203     C                   MOVEL(p)  DATCOR        KTBEKE2
009900160112     C                   CLEAR                   DAOA
010000160203     C                   SETOFF                                       33
010100160203     C     KEYTBE01_P    SETGT     tntbe01l
010200160112     C                   IF        %found(tntbe01l)
010300160203     C                   DOW       not %eof(tntbe01l) AND
010400160203     C                             *IN33 = *OFF
010500160203     C                   READP     tntbe01l
010600160203     C                   IF        TBEKE1  = KTBEKE1
010700160203     C                   IF        TBEKE2 <= KTBEKE2 AND
010800160203     C                             TBEATB  = *blanks
010900160203     C                   SETON                                        33
011000160203     C                   ENDIF
011100160203     C                   ELSE
011200160203     C                   LEAVE
011300160203     C                   ENDIF
011400160203     C                   ENDDO
011500160203     C                   ENDIF
011600160203     C*
011700160203     C                   IF        *IN33
011800160129     C                   MOVEL(P)  TBEUNI        DAOA
011900090514     C*
012000090514     C* Verifico correttezza formale sotto-cartella
012100090514     C                   IF        SUBFLDR <> *blanks AND
012200090514     C                             %subst(%trim(SUBFLDR):%len(%trim(SUBFLDR)):1)
012300090514     C                             <> '/'
012400090514     C                   EVAL      SUBFLDR = %trim(SUBFLDR) + '/'
012500090514     C                   ENDIF
012600160203     C*
012700160203     C* Effettuo mappatura tra struttura folder DocFlow e ArchiDoc
012800160203     C                   SELECT
012900160203     C                   WHEN      SUBFLDR = 'FATD/'
013000160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/DISTINTE/'
013100160203     C                   WHEN      SUBFLDR = 'FATA/'
013200160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/FATSEDE/'
013300160203     C                   WHEN      SUBFLDR = 'FAT9/'
013400160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/FATFILIALE/'
013500160203     C                   WHEN      SUBFLDR = 'FATE/'
013600160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/'
013700160219     C                   WHEN      SUBFLDR = 'FTMB/'
013800160219     C                   EVAL      SUBFLDR_ARCHI = 'FATMAN/'
013900160219     C                   WHEN      SUBFLDR = 'FTMX/'
014000160219     C                   EVAL      SUBFLDR_ARCHI = 'FATMAN/'
014100160606     C                   WHEN      SUBFLDR = 'ABON/'
014200160606     C                   EVAL      SUBFLDR_ARCHI = 'MODBON/'
014300160606     C                   WHEN      SUBFLDR = 'LGIA/'
014400160606     C                   EVAL      SUBFLDR_ARCHI = 'GIACENZE/'
014500160606     C                   WHEN      SUBFLDR = 'LGIC/'
014600160606     C                   EVAL      SUBFLDR_ARCHI = 'GIACENZE/'
014700160606     C                   WHEN      SUBFLDR = 'ABAR/'
014800160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
014900160606     C                   WHEN      SUBFLDR = 'ANLT/'
015000160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
015100160606     C                   WHEN      SUBFLDR = 'ADOC/'
015200160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
015300160205     C                   OTHER
015400160205     C                   EVAL      SUBFLDR_ARCHI = SUBFLDR
015500160203     C                   ENDSL
015600001006     C*
015700091211     C* Valorizzo la DS pgm utilità TRBMFCC
015800091211     C                   CLEAR                   TRBMFCDS
015900030404     C                   EVAL      DFCFNAM  = FILNAM
016000030404     C                   EVAL      DFCJNAM  = JOBNAM
016100030404     C                   EVAL      DFCUNAM  = USRNAM
016200030404     C                   EVAL      DFCJNUM  = JOBNUM
016300030404     C                   EVAL      DFCFNUM  = NUMFIL
016400091211     C                   EVAL      DFCFOUT  = 'TRBMUWWF'
016500040907     C                   EVAL      DFCLOUT  = 'QTEMP'
016600030514     C                   EVAL      DFCFOPT  = 'R'
016700150618     C*
016800150618     C* Verifico composizione del codice opzione secondo la seguente sintassi:
016900160112     C*
017000150618     C* - 1° byte => pilota popolamento ambiente Archidoc
017100150618     C*              valori possibili 'N' = NO
017200150618     C*                      diversamente = SI
017300160112     C*
017400150618     C* - 2° byte => pilota popolamento ambiente DocFlow
017500150618     C*              valori possibili 'T' = TEST
017600170605     C*                               'P' = PROD
017700170605     C*                               'X' = PROD XML
017800150618     C*                      diversamente = NESSUNO
017900160112     C*
018000150618     C* - 3° byte => pilota popolamento ambiente DocFlow Storico
018100150618     C*              valori possibili 'S' = SI
018200150618     C*                      diversamente = NO
018300160112     C*
018400160112     C                   MOVEL     *blanks       ROOT            100
018500160119     C                   MOVEL     *blanks       ESITO
018600150618     C*
018700150618     C* Eseguo le chiamate richieste secondo codice opzione ricevuto in input
018800160129     C                   IF        §AOAARCHI <> 'N'
018900160115     C                   EVAL      ROOT = '/Ottico/'
019000170605     C                   CALL(e)   'TRBMAP05C'
019100090514     C                   PARM                    IFSNAM
019200030401     C                   PARM                    FILNAM
019300030401     C                   PARM                    JOBNAM
019400030401     C                   PARM                    USRNAM
019500030401     C                   PARM                    JOBNUM
019600030401     C                   PARM                    NUMFIL
019700150618     C                   PARM                    ROOT
019800160203     C                   PARM                    SUBFLDR_ARCHI
019900091211     C                   PARM                    TRBMFCDS
020000170605     C                   PARM                    FILETYPE
020100160112     C                   PARM                    FILEOUT
020200160112     C                   PARM      *blanks       DATAORA          15
020300160115     C                   PARM                    FLGSUBFLDR
020400160120     C                   PARM                    FLGREPLACE
020500170605     C                   PARM                    FLGDELSPL
020600160119     C                   PARM                    ESITO
020700150618     C                   ENDIF
020800001006     C*
020900160129     C                   IF        §AOADOCFLW = 'T'
021000160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
021100160513     C                                    'TEST/'
021200170605     C                   CALL(e)   'TRBMAP05C'
021300150618     C                   PARM                    IFSNAM
021400150618     C                   PARM                    FILNAM
021500150618     C                   PARM                    JOBNAM
021600150618     C                   PARM                    USRNAM
021700150618     C                   PARM                    JOBNUM
021800150618     C                   PARM                    NUMFIL
021900150618     C                   PARM                    ROOT
022000150618     C                   PARM                    SUBFLDR
022100150618     C                   PARM                    TRBMFCDS
022200170605     C                   PARM                    FILETYPE
022300160112     C                   PARM                    FILEOUT
022400160112     C                   PARM      *blanks       DATAORA          15
022500160115     C                   PARM                    FLGSUBFLDR
022600160120     C                   PARM                    FLGREPLACE
022700170605     C                   PARM                    FLGDELSPL
022800160119     C                   PARM                    ESITO
022900150618     C                   ENDIF
023000160120     C*
023100160120     C* - Per "sicurezza" se richiesto effettuo prima la storicizzazione
023200160129     C                   IF        §AOADOCFLS = 'S'
023300160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
023400160513     C                                    'ArchivioSpool/'
023500170605     C                   CALL(e)   'TRBMAP05C'
023600160120     C                   PARM                    IFSNAM
023700160120     C                   PARM                    FILNAM
023800160120     C                   PARM                    JOBNAM
023900160120     C                   PARM                    USRNAM
024000160120     C                   PARM                    JOBNUM
024100160120     C                   PARM                    NUMFIL
024200160120     C                   PARM                    ROOT
024300160120     C                   PARM                    SUBFLDR
024400160120     C                   PARM                    TRBMFCDS
024500170605     C                   PARM                    FILETYPE
024600160120     C                   PARM                    FILEOUT
024700160120     C                   PARM      wDATAORA      DATAORA          15
024800160120     C                   PARM                    FLGSUBFLDR
024900171011     C                   PARM      'S'           wFLGREPLACE
025000170605     C                   PARM                    FLGDELSPL
025100160120     C                   PARM                    ESITO
025200160120     C                   ENDIF
025300150618     C*
025400160129     C                   IF        §AOADOCFLW = 'P'
025500160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
025600160513     C                                    'Corrente/'
025700170605     C                   CALL(e)   'TRBMAP05C'
025800150618     C                   PARM                    IFSNAM
025900150618     C                   PARM                    FILNAM
026000150618     C                   PARM                    JOBNAM
026100150618     C                   PARM                    USRNAM
026200150618     C                   PARM                    JOBNUM
026300150618     C                   PARM                    NUMFIL
026400150618     C                   PARM                    ROOT
026500150618     C                   PARM                    SUBFLDR
026600150618     C                   PARM                    TRBMFCDS
026700170605     C                   PARM                    FILETYPE
026800160112     C                   PARM                    FILEOUT
026900160112     C                   PARM      *blanks       DATAORA          15
027000160115     C                   PARM                    FLGSUBFLDR
027100160120     C                   PARM                    FLGREPLACE
027200170605     C                   PARM                    FLGDELSPL
027300160119     C                   PARM                    ESITO
027400150618     C                   ENDIF
027500170605     C*
027600170605     C                   IF        §AOADOCFLW = 'X'
027700170605     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
027800170605     C                                    'CorrenteXML/'
027900170605     C                   CALL(e)   'TRBMAP05C'
028000170605     C                   PARM                    IFSNAM
028100170605     C                   PARM                    FILNAM
028200170605     C                   PARM                    JOBNAM
028300170605     C                   PARM                    USRNAM
028400170605     C                   PARM                    JOBNUM
028500170605     C                   PARM                    NUMFIL
028600170605     C                   PARM                    ROOT
028700170605     C                   PARM                    SUBFLDR
028800170605     C                   PARM                    TRBMFCDS
028900170605     C                   PARM                    FILETYPE
029000170605     C                   PARM                    FILEOUT
029100170605     C                   PARM      *blanks       DATAORA          15
029200170605     C                   PARM                    FLGSUBFLDR
029300170605     C                   PARM                    FLGREPLACE
029400170605     C                   PARM                    FLGDELSPL
029500170605     C                   PARM                    ESITO
029600170605     C                   ENDIF
029700160127     C*
029800160127     C* Se richiesto nei parametri di input => loggo richiesta corrente
029900160127     C                   IF        FLGLOG = 'S'
030000160127     C                   CLEAR                   WFMAF05000
030100160127     C                   EVAL      AF05DTEXER = %timestamp()
030200160129     C                   EVAL      AF05OPZ    = DAOA
030300160127     C                   EVAL      AF05JOBNM  = jobname
030400160127     C                   EVAL      AF05JOBUT  = jobuser
030500160127     C                   EVAL      AF05JOBNR  = jobnumber
030600160127     C                   EVAL      AF05SPLNM  = FILNAM
030700160127     C                   EVAL      AF05SPLJOB = JOBNAM
030800160127     C                   EVAL      AF05SPLUT  = USRNAM
030900160127     C                   EVAL      AF05SPLJNR = JOBNUM
031000160127     C                   EVAL      AF05SPLNR  = NUMFIL
031100160127     C                   EVAL      AF05ESITO  = ESITO
031200160127     C                   WRITE(e)  WFMAF05000
031300160127     C                   ENDIF
031400150618     C*
031500160129     C                   ELSE
031600160129     C                   EVAL      ESITO = '2'
031700160129     C                   ENDIF
031800160129     C*
031900090513     C                   SETON                                        LR
