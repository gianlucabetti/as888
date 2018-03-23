000100090717     h datedit(*ymd/) dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700090717     Fazcpr02l  iF   E           K DISK
000800090724     D IFile           S              5
000900080620     D RTNesito        S             10I 0
001000080620     D RTNopcode       S             10
001100080620     D RTNstatus       S             10I 0
001200090717     D depver          S                   LIKE(cprver)
001300080620
001400080620      //********************************************************************************************
001500080620      //
001600080620      // Definizione parametri procedura.
001700080620      //
001800080620      //********************************************************************************************
001900080620     C     *ENTRY        PLIST
002000090724     C                   PARM                    IFile
002100080620     C                   PARM                    rtnesito
002200080620     C                   PARM                    rtnopcode
002300080620     C                   PARM                    rtnstatus
002400080620     c*
002500080620      /FREE
002600080620
002700080620       //*******************************************************************************************
002800080620       //
002900080620       // Main.
003000080620       //
003100080620       //*******************************************************************************************
003200080620       CLEAR rtnesito;
003300080620       CLEAR rtnopcode;
003400080620       CLEAR rtnstatus;
003500080620       *INLR = *ON;
003600080620
003700080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003800090717
003900090717      /END-FREE
004000090724
004100090724      * se non è stato passato il file da estrarre ritorno errore
004200090724     c                   IF        IFile = *blanks
004300090724     c                   eval      RTNstatus = 99
004400090724     c                   eval      RTNopcode = 'ERRORE '
004500090724     c                   eval      RTNesito  = -1
004600090724     c                   return
004700090724     c                   ENDIF
004800110110
004900110110
005000110110      * determino la data corrente
005100110110     c                   z-add     *zeros        datcor            8 0
005200110110     c                   eval      datcor = %dec(%date() : *ISO)
005300090717
005400110110
005500090717      * recupero versione cappario alla data odierna
005600090717     c                   clear                   depver
005700090717     c     *loval        SETLL     azcpr02l
005800090717     c                   READ      azcpr02l
005900090717do  2c                   DOW       NOT %eof
006000090717if  3c                   IF        cpratb=*blanks
006100110110if  4c                   IF        datcor >=cprdde and datcor <=cprdsc          *cap in decorrenza
006200090717     c                   Z-ADD     cprver        depver                         *versione
006300091119     c                   LEAVE
006400090717e   4c                   ENDIF
006500090717e   3c                   ENDIF
006600090717     c                   READ      azcpr02l
006700090717e   2c                   ENDDO                                                  *fine versioni
006800080620
006900090717      /FREE
007000091119
007100091119       // ritorno il file richiesto
007200091119       IF  IFile = 'AZCPR';
007300091119         exec sql
007400091119          declare azcpr cursor for
007500091119          select azcpr00f.* from azcpr00f
007600091119          where cprver = :depver
007700091119          for read only;
007800091119
007900091119         exec sql OPEN azcpr;
008000091119
008100091119         select;
008200091119           When sqlCode = 100;
008300091119             RTNstatus = sqlCode;
008400091119             RTNopcode = 'SQLCODE';
008500091119             RTNesito  = -1;
008600091119             RETURN;
008700091119           When sqlCode < 0;
008800091119             RTNstatus = sqlCode;
008900091119             RTNopcode = 'SQLCODE';
009000091119             RTNesito  = -1;
009100091119             RETURN;
009200091119         Endsl;
009300091119
009400091119         exec sql SET RESULT SETS CURSOR azcpr;
009500091119
009600091119         IF  sqlCode < 0;
009700091119           RTNstatus = sqlCode;
009800091119           RTNopcode = 'SQLCODE';
009900091119           RTNesito  = -1;
010000091119           RETURN;
010100091119         ENDIF;
010200091119
010300091119       ENDIF;
010400091119
010500090724
010600090724       // ritorno il file richiesto
010700090724       IF  IFile = 'AZCPL';
010800090724         exec sql
010900090724          declare azcpl cursor for
011000090724          select azcpl00f.* from azcpl00f
011100090724          where cplver = :depver
011200090724          for read only;
011300090717
011400090724         exec sql OPEN azcpl;
011500090717
011600090724         select;
011700090724           When sqlCode = 100;
011800090724             RTNstatus = sqlCode;
011900090724             RTNopcode = 'SQLCODE';
012000090724             RTNesito  = -1;
012100090724             RETURN;
012200090724           When sqlCode < 0;
012300090724             RTNstatus = sqlCode;
012400090724             RTNopcode = 'SQLCODE';
012500090724             RTNesito  = -1;
012600090724             RETURN;
012700090724         Endsl;
012800090717
012900090724         exec sql SET RESULT SETS CURSOR azcpl;
013000090717
013100090724         IF  sqlCode < 0;
013200090724           RTNstatus = sqlCode;
013300090724           RTNopcode = 'SQLCODE';
013400090724           RTNesito  = -1;
013500090724           RETURN;
013600090724         ENDIF;
013700090724
013800090724       ENDIF;
013900090724
014000090724       IF  IFile = 'AZCPC';
014100090724         exec sql
014200090724          declare azcpc cursor for
014300090724          select azcpc00f.* from azcpc00f
014400090724          where cpcver = :depver
014500090724          for read only;
014600090724
014700090724         exec sql OPEN azcpc;
014800090724
014900090724         select;
015000090724           When sqlCode = 100;
015100090724             RTNstatus = sqlCode;
015200090724             RTNopcode = 'SQLCODE';
015300090724             RTNesito  = -1;
015400090724             RETURN;
015500090724           When sqlCode < 0;
015600090724             RTNstatus = sqlCode;
015700090724             RTNopcode = 'SQLCODE';
015800090724             RTNesito  = -1;
015900090724             RETURN;
016000090724         Endsl;
016100090724
016200090724         exec sql SET RESULT SETS CURSOR azcpc;
016300090724
016400090724         IF  sqlCode < 0;
016500090724           RTNstatus = sqlCode;
016600090724           RTNopcode = 'SQLCODE';
016700090724           RTNesito  = -1;
016800090724           RETURN;
016900090724         ENDIF;
017000090724
017100090724       ENDIF;
017200090717
017300090717       RETURN;
017400080620
