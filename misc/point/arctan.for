	FUNCTION ATAN3D(X,Y)

C	atan2 + defines atan2d(0,0)=0.
c gives angle in degrees

	REAL X,Y

	IF( X.EQ.0. .AND. Y.EQ.0. ) THEN
		ATAN3d = 0.
	ELSE
		ATAN3d = ATAN2d(X,Y)
	END IF
	RETURN
	END

	FUNCTION ATAN3(X,Y)
C************************************************************
C	atan2 + defines atan2(0,0)=0.
C************************************************************
	REAL X,Y

	IF( X.EQ.0. .AND. Y.EQ.0. ) THEN
		ATAN3 = 0.
	ELSE
		ATAN3 = ATAN2(X,Y)
	END IF
	RETURN
	END
