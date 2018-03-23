000100140912     FPRTF198   O    f  198        PRINTER
000200140912     F                                     oflind(*inoa)
000300140918 CPY  /COPY GAITRASRC/SRCUB,UBTL05D02
000400140911       // -?Stringa SQL da eseguire?
000500140911     d wSql            s           4096    inz  varying
000600140911     D wTS1            s               z
000700140911     D wTS2            s               z
000800140911     D wTS3            s               z
000900140911     D wRcdLetti       s              9s 0
001000140911     D wCallOK         s              9s 0
001100140912     D wCallErr        s              9s 0
001200140912     D Msg             s            198
001300140911
001400140912     D pIn_TpStp       S             10A
001500140912
001600140912     D   pInNazione    S              3A
001700140911     D   pInProvincia  S              4A
001800140911     D   pInComune     S            128A
001900140911     D   pInLocalita   S            128A
002000140911     D   pInCAP        S              9A
002100140911     D   pInVia        S            128A
002200140911     D   pInCivico     S             10A
002300140918     D   pInScore      S             10A
002400140918     D   pOutEsito     S              1A
002500140918     D   pOutEsitoDes  S            128A
002600140918     D   pOutTotFound  S              4S 0
002700140918     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)       10 * 630 della DS
002800140912
002900140912     d data            s               d
003000140912     d ora             s               t
003100140912
003200140912     D* campi per stampa
003300140912     D LNA             S              3A
003400140912     D Provincia       S              2A
003500140912     D Comune          S             20A
003600140912     D CAP             S              5A
003700140912     D Via             S             32A
003800140912     D OutLivNorm      S              1A
003900140912     D OutRegione      S             20A
004000140912     D OutProv         S              2A
004100140912     D OutCAP          S              5A
004200140912     D OutComune       S             20A
004300140912     D OutVia          S             30A
004400140912     D OutCivico       S              5A
004500140912     D Esito           S              5A
004600140912
004700140912     d Status         sds
004800140912     d  SDSpgm           *proc
004900140912     d  SDSprm           *parms
005000140912     d  SDSuser              254    263
005100140911
005200140911     D TITAS00F      e ds
005300140911     D TITASDS         ds
005400140912     D  SQL_ind                            like(tasind)
005500140911     D  SQL_lod                            like(taslod)
005600140911     D  SQL_cad                            like(tascad)
005700140911     D  SQL_prd                            like(tasprd)
005800140912     D  SQL_lna                            like(taslna)
005900140911
006000140912     C     *entry        plist
006100140912     C                   parm                    pIn_TpStp
006200140912
006300140911      /free
006400140912       data = %date();
006500140912       ora = %time();
006600140911
006700140911       wSQL =
006800140912             'select tasind, taslod, tascad, tasprd, taslna from titas00f +
006900140911              where tasnzd='' '' and tascad in +
007000140911              (select cpccap from azcpc00f +
007100140911              where cpcver=44 and cpcnar='' '' and cpcflg=''S'' +
007200140912              and cpcfit=''N'') +
007300140912              order by taslna, tasprd, taslod, tascad, tasind' ;
007400140911       exec sql  prepare S1  from :wSql;
007500140911       exec sql  declare C1  cursor for S1;
007600140911       exec sql  open C1;
007700140911
007800140911       WTS1 = %timestamp();
007900140912
008000140912       except testata;
008100140911
008200140911       DoU  SQLCode = 100 or SQLCode < 0;
008300140911         exec sql  fetch next  from C1  into :TITASds;
008400140911
008500140912         if *inOA = *on;
008600140912           except testata;
008700140912         endif;
008800140912
008900140911         select;
009000140911         // errore
009100140911         when SQLCode < 0;
009200140911           dsply 'ERRORE SQL';
009300140912           // - Chiusura cursore (visto che esco dal pgm)
009400140911           exec sql   close C1;
009500140911           *inlr = *on;
009600140911           return;
009700140911         // nessuna lettura
009800140911         when SQLCode = 100;
009900140912           // esco dal ciclo
010000140911         // lettura
010100140911         when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
010200140911           wRcdLetti += 1;
010300140911           exsr TL05R02;
010400140912           //chiamate corrette
010500140918           if pOutResults(1).Esatto = '0' and pOutResults(1).LivNorm = 'V' and
010600140911              pOutEsito <> 'E';
010700140911             wCallOK += 1;
010800140912             //stampo se richiesto
010900140912             if pIn_TpStp   = 'PRINTALL' or
011000140912                pIn_TpStp   = 'PRINTOK';
011100140912                LNA         = %editc(sql_LNA:'X');
011200140912                Provincia   = pInProvincia  ;
011300140912                Comune      = pInComune     ;
011400140912                CAP         = pInCAP        ;
011500140912                Via         = pInVia        ;
011600140918                OutLivNorm  = pOutResults(1).LivNorm     ;
011700140918                OutRegione  = pOutResults(1).Regione     ;
011800140918                OutProv     = pOutResults(1).Prov        ;
011900140918                OutCAP      = pOutResults(1).CAP         ;
012000140918                OutComune   = pOutResults(1).Comune      ;
012100140918                OutVia      = pOutResults(1).Via         ;
012200140918                OutCivico   = pOutResults(1).Civico      ;
012300140912                Esito       = 'OK   '       ;
012400140912                except dettaglio;
012500140912             endif;
012600140912           //chiamate in errore
012700140912           else;
012800140912             wCallErr += 1;
012900140912             //stampo se richiesto
013000140912             if pIn_TpStp = 'PRINTALL' or
013100140912                pIn_TpStp = 'PRINTOK';
013200140912                LNA         = %editc(sql_LNA:'X');
013300140912                Provincia   = pInProvincia  ;
013400140912                Comune      = pInComune     ;
013500140912                CAP         = pInCAP        ;
013600140912                Via         = pInVia        ;
013700140918                OutLivNorm  = pOutResults(1).LivNorm     ;
013800140918                OutRegione  = pOutResults(1).Regione     ;
013900140918                OutProv     = pOutResults(1).Prov        ;
014000140918                OutCAP      = pOutResults(1).CAP         ;
014100140918                OutComune   = pOutResults(1).Comune      ;
014200140918                OutVia      = pOutResults(1).Via         ;
014300140918                OutCivico   = pOutResults(1).Civico      ;
014400140912                Esito       = '   KO'       ;
014500140912                except dettaglio;
014600140912             endif;
014700140912           endif;
014800140911         // fine lettura
014900140911         other;
015000140911           // non faccio nulla
015100140911         endsl;
015200140911
015300140911       enddo;
015400140911
015500140911       //emetto risultati
015600140911       WTS2 = %timestamp();
015700140911       Msg = 'tot rcd letti ' + %char(wRcdLetti);
015800140911       Msg = %trim(Msg) + ' di cui normalizzato OK: ' +
015900140911                 %char(wCallOK);
016000140912       except totali;
016100140912       Msg = 'secondi impiegati: ' +
016200140912             %char(%diff(wTS2:wTS1:*S));
016300140912       except totali;
016400140911
016500140912       // - Chiusura cursore (visto che esco dal pgm)
016600140912       exec sql   close C1;
016700140912
016800140911      /END-free
016900140911     C                   seton                                        lr
017000140911
017100140911     C     TL05R02       BEGSR
017200140912
017300140911     C                   EVAL      pInNazione   = *blank
017400140911     C                   EVAL      pInProvincia = sql_prd
017500140911     C                   EVAL      pInComune    = sql_lod
017600140911     C                   EVAL      pInLocalita  = *blank
017700140911     C                   EVAL      pInCAP       = sql_cad
017800140911     C                   EVAL      pInVia       = sql_ind
017900140911     C                   EVAL      pInCivico    = *blanks
018000140911     C*
018100140911     C                   CALL      'UBTL05R02'
018200140911     C                   PARM                    pInNazione
018300140911     C                   PARM                    pInProvincia
018400140911     C                   PARM                    pInComune
018500140911     C                   PARM                    pInLocalita
018600140911     C                   PARM                    pInCAP
018700140911     C                   PARM                    pInVia
018800140911     C                   PARM                    pInCivico
018900140918     C                   PARM                    pInScore
019000140918     C                   PARM                    pOutEsito
019100140918     C                   PARM                    pOutEsitoDes
019200140918     C                   PARM                    pOutTotFound
019300140918     C                   PARM                    pOutResults
019400140911     C*
019500140911     C                   ENDSR
019600140912     OPRTF198   E            testata           2
019700140912     O                       SDSUSER
019800140912     O                                          101 '* Stress-test e misurazion'
019900140912     O                                          123 'e performance TELLUS *'
020000140912     O                       data               196
020100140912     O          E            testata     1
020200140912     O                       SDSPGM
020300140912     O                       ORA                196
020400140912     O          E            testata     1
020500140912     O                                              'Tipo stampa: '
020600140912     O                       pIn_TpStp
020700140912     O          E            testata     1
020800140912     O                                            3 'LNA'
020900140912     O                                            8 'Prv'
021000140912     O                                           17 'Località'
021100140912     O                                           34 'CAP'
021200140912     O                                           47 'Indirizzo'
021300140912     O                                           76 '- LN'
021400140912     O                                           84 'Regione'
021500140912     O                                          102 'Prv'
021600140912     O                                          106 'CAP'
021700140912     O                                          116 'Comune'
021800140912     O                                          141 'Indirizzo'
021900140912     O                                          170 'Civico'
022000140912     O                                          178 '- OK/KO'
022100140912     O          E            dettaglio   1
022200140912     O                       LNA                  3
022300140912     O                       Provincia            7
022400140912     O                       Comune              29
022500140912     O                       CAP                 36
022600140912     O                       Via                 70
022700140912     O                                           73 '|'
022800140912     O                       OutLivNorm          75
022900140912     O                       OutRegione          97
023000140912     O                       OutProv            101
023100140912     O                       OutCAP             108
023200140912     O                       OutComune          130
023300140912     O                       OutVia             162
023400140912     O                       OutCivico          169
023500140912     O                       Esito              178
023600140912     O          E            totali      1
023700140912     O                       msg                198
