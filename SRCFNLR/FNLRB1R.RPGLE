000100040206     h decedit('0,') datedit(*DMY/) option(*nodebugio)
000200080117      * FNLRB1R *----------------------------------------------------*
000300080117      *         PULIZIA LOG Spunte da pistole radio                  *
000400040206      *--------------------------------------------------------------*
000500080117     fFIPSL00F  uf   e             disk
000600040206      *---------------------------------------------------------------*
000700040206     d KPJBA         e ds
000800040206     d PARAM           ds
000900040206     d  DatPulA                5     12
001000040206      *
001100040206      *---------------------------------------------------------------*
001200040206     c     *ENTRY        plist
001300040206     c                   parm                    KPJBA
001400080118      /FREE
001500080118
001600080118       param=kpjbu;
001700080118
001800080118       // Ciclo di lettura e di cancellazione record per Data Spunta
001900080118       read fipsl00f;
002000080118
002100080118do  1  DOW  NOT %eof(fipsl00f);
002200080118
002300080118if  2       if    %subst(psldatora:1:8) <= DatPula;
002400080118                  DELETE    fipsl000;
002500080118e   2       endif;
002600080118
002700080118       read fipsl00f;
002800080118e   1  ENDDO;
002900080118
003000080118       //Fine
003100080118        *inlr=*on;
