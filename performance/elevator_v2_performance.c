#define id2iperf_return(expr, stmts) __typeof__(expr) ____RetTmp = expr; stmts; return ____RetTmp;
#include "/home/flo/TypeChef/Hercules/performance/includes.c"
#include "/home/flo/TypeChef/Hercules/performance/perf_measuring.c"
#include "/home/flo/TypeChef/ifdeftoif/id2i_optionstruct.h"
int usleep(int usec );
void exit(int status );
int __GUIDSL_ROOT_PRODUCTION;
void select_helpers();
int valid_product();
int weight =  0;
int maximumWeight =  100;
int getWeight(int person );
int getOrigin(int person );
int getDestination(int person );
void enterElevator(int person );
int isFloorCalling(int floorID );
void resetCallOnFloor(int floorID );
void callOnFloor(int floorID );
int isPersonOnFloor(int person , int floor );
void initPersonOnFloor(int person , int floor );
void removePersonFromFloor(int person , int floor );
int isTopFloor(int floorID );
void processWaitingPersons(int floorID );
void initFloors();
void timeShift();
int isBlocked();
void printState();
int isEmpty();
int isAnyLiftButtonPressed();
int buttonForFloorIsPressed(int floorID );
void initTopDown();
void initBottomUp();
int areDoorsOpen();
int getCurrentFloorID();
int isIdle();
int executiveFloor;
int isExecutiveFloorCalling();
int isExecutiveFloor(int floorID );
int blocked;
int currentHeading =  1;
int currentFloorID =  0;
int persons_0;
int persons_1;
int persons_2;
int persons_3;
int persons_4;
int persons_5;
int doorState =  1;
int floorButtons_0;
int floorButtons_1;
int floorButtons_2;
int floorButtons_3;
int floorButtons_4;
void initTopDown()  {
  (currentFloorID = 4);
  (currentHeading = 0);
  (floorButtons_0 = 0);
  (floorButtons_1 = 0);
  (floorButtons_2 = 0);
  (floorButtons_3 = 0);
  (floorButtons_4 = 0);
  (persons_0 = 0);
  (persons_1 = 0);
  (persons_2 = 0);
  (persons_3 = 0);
  (persons_4 = 0);
  (persons_5 = 0);
  initFloors();
}
void initBottomUp()  {
  (currentFloorID = 0);
  (currentHeading = 1);
  (floorButtons_0 = 0);
  (floorButtons_1 = 0);
  (floorButtons_2 = 0);
  (floorButtons_3 = 0);
  (floorButtons_4 = 0);
  (persons_0 = 0);
  (persons_1 = 0);
  (persons_2 = 0);
  (persons_3 = 0);
  (persons_4 = 0);
  (persons_5 = 0);
  initFloors();
}
int isBlocked__before__overloaded()  {
  return 0;
}
int isBlocked__role__overloaded()  {
  usleep(100);
  return blocked;
}
int isBlocked()  {
  if (id2i_overloaded) {
    id2iperf_time_before("OVERLOADED");
    id2iperf_return(isBlocked__role__overloaded(), id2iperf_time_after(1));
  }  
  if ((! id2i_overloaded)) {
    id2iperf_time_before("!OVERLOADED");
    id2iperf_return(isBlocked__before__overloaded(), id2iperf_time_after(1));
  }  
}
void enterElevator__before__weight(int p )  {
  if ((p == 0)) {
    (persons_0 = 1);
  } 
  else if ((p == 1)) {
    (persons_1 = 1);
  }
  
  else if ((p == 2)) {
    (persons_2 = 1);
  }
  
  else if ((p == 3)) {
    (persons_3 = 1);
  }
  
  else if ((p == 4)) {
    (persons_4 = 1);
  }
  
  else if ((p == 5)) {
    (persons_5 = 1);
  } 
}
void enterElevator__role__weight(int p )  {
  usleep(100);
  enterElevator__before__weight(p);
  (weight = (weight + getWeight(p)));
}
void enterElevator(int p )  {
  enterElevator__before__weight(p);
  if (id2i_weight) {
    id2iperf_time_before("WEIGHT");
    usleep(100);
    (weight = (weight + getWeight(p)));
    id2iperf_time_after(2);
  }  
}
void leaveElevator__before__weight(int p )  {
  if ((p == 0)) {
    (persons_0 = 0);
  } 
  else if ((p == 1)) {
    (persons_1 = 0);
  }
  
  else if ((p == 2)) {
    (persons_2 = 0);
  }
  
  else if ((p == 3)) {
    (persons_3 = 0);
  }
  
  else if ((p == 4)) {
    (persons_4 = 0);
  }
  
  else if ((p == 5)) {
    (persons_5 = 0);
  } 
}
void leaveElevator__role__weight(int p )  {
  leaveElevator__before__weight(p);
  (weight = (weight - getWeight(p)));
}
void leaveElevator__before__empty(int p )  {
  leaveElevator__before__weight(p);
  if (id2i_weight) {
    id2iperf_time_before("WEIGHT");
    (weight = (weight - getWeight(p)));
    id2iperf_time_after(1);
  }  
}
void leaveElevator__role__empty(int p )  {
  usleep(50);
  leaveElevator__before__empty(p);
  if (isEmpty()) {
    (floorButtons_0 = 0);
    (floorButtons_1 = 0);
    (floorButtons_2 = 0);
    (floorButtons_3 = 0);
    (floorButtons_4 = 0);
  }  
}
void leaveElevator(int p )  {
  leaveElevator__before__empty(p);
  if (id2i_empty) {
    id2iperf_time_before("EMPTY");
    usleep(50);
    if (isEmpty()) {
      (floorButtons_0 = 0);
      (floorButtons_1 = 0);
      (floorButtons_2 = 0);
      (floorButtons_3 = 0);
      (floorButtons_4 = 0);
    }  
    id2iperf_time_after(2);
  }  
}
void pressInLiftFloorButton(int floorID )  {
  if ((floorID == 0)) {
    (floorButtons_0 = 1);
  } 
  else if ((floorID == 1)) {
    (floorButtons_1 = 1);
  }
  
  else if ((floorID == 2)) {
    (floorButtons_2 = 1);
  }
  
  else if ((floorID == 3)) {
    (floorButtons_3 = 1);
  }
  
  else if ((floorID == 4)) {
    (floorButtons_4 = 1);
  } 
}
void resetFloorButton(int floorID )  {
  if ((floorID == 0)) {
    (floorButtons_0 = 0);
  } 
  else if ((floorID == 1)) {
    (floorButtons_1 = 0);
  }
  
  else if ((floorID == 2)) {
    (floorButtons_2 = 0);
  }
  
  else if ((floorID == 3)) {
    (floorButtons_3 = 0);
  }
  
  else if ((floorID == 4)) {
    (floorButtons_4 = 0);
  } 
}
int getCurrentFloorID()  {
  return currentFloorID;
}
int areDoorsOpen()  {
  return doorState;
}
int buttonForFloorIsPressed(int floorID )  {
  if ((floorID == 0)) {
    return floorButtons_0;
  } 
  else if ((floorID == 1)) {
    return floorButtons_1;
  }
  
  else if ((floorID == 2)) {
    return floorButtons_2;
  }
  
  else if ((floorID == 3)) {
    return floorButtons_3;
  }
  
  else if ((floorID == 4)) {
    return floorButtons_4;
  } 
  else {
    return 0;
  }
}
int getCurrentHeading()  {
  return currentHeading;
}
int isEmpty()  {
  if ((persons_0 == 1)) {
    return 0;
  } 
  else if ((persons_1 == 1)) {
    return 0;
  }
  
  else if ((persons_2 == 1)) {
    return 0;
  }
  
  else if ((persons_3 == 1)) {
    return 0;
  }
  
  else if ((persons_4 == 1)) {
    return 0;
  }
  
  else if ((persons_5 == 1)) {
    return 0;
  } 
  return 1;
}
int anyStopRequested()  {
  if (isFloorCalling(0)) {
    return 1;
  } 
  else if (floorButtons_0) {
    return 1;
  }
  
  else if (isFloorCalling(1)) {
    return 1;
  }
  
  else if (floorButtons_1) {
    return 1;
  }
  
  else if (isFloorCalling(2)) {
    return 1;
  }
  
  else if (floorButtons_2) {
    return 1;
  }
  
  else if (isFloorCalling(3)) {
    return 1;
  }
  
  else if (floorButtons_3) {
    return 1;
  }
  
  else if (isFloorCalling(4)) {
    return 1;
  }
  
  else if (floorButtons_4) {
    return 1;
  } 
  return 0;
}
int isIdle()  {
  return (anyStopRequested() == 0);
}
int stopRequestedInDirection__before__twothirdsfull(int dir , int respectFloorCalls , int respectInLiftCalls )  {
  if ((dir == 1)) {
    if (isTopFloor(currentFloorID)) {
      return 0;
    }  
    if (((currentFloorID < 0) && respectFloorCalls && isFloorCalling(0))) {
      return 1;
    } 
    else if (((currentFloorID < 0) && respectInLiftCalls && floorButtons_0)) {
      return 1;
    }
    
    else if (((currentFloorID < 1) && respectFloorCalls && isFloorCalling(1))) {
      return 1;
    }
    
    else if (((currentFloorID < 1) && respectInLiftCalls && floorButtons_1)) {
      return 1;
    }
    
    else if (((currentFloorID < 2) && respectFloorCalls && isFloorCalling(2))) {
      return 1;
    }
    
    else if (((currentFloorID < 2) && respectInLiftCalls && floorButtons_2)) {
      return 1;
    }
    
    else if (((currentFloorID < 3) && respectFloorCalls && isFloorCalling(3))) {
      return 1;
    }
    
    else if (((currentFloorID < 3) && respectInLiftCalls && floorButtons_3)) {
      return 1;
    }
    
    else if (((currentFloorID < 4) && respectFloorCalls && isFloorCalling(4))) {
      return 1;
    }
    
    else if (((currentFloorID < 4) && respectInLiftCalls && floorButtons_4)) {
      return 1;
    } 
    else {
      return 0;
    }
  }  
  else {
    if ((currentFloorID == 0)) {
      return 0;
    }  
    if (((currentFloorID > 0) && respectFloorCalls && isFloorCalling(0))) {
      return 1;
    } 
    else if (((currentFloorID > 0) && respectInLiftCalls && floorButtons_0)) {
      return 1;
    }
    
    else if (((currentFloorID > 1) && respectFloorCalls && isFloorCalling(1))) {
      return 1;
    }
    
    else if (((currentFloorID > 1) && respectInLiftCalls && floorButtons_1)) {
      return 1;
    }
    
    else if (((currentFloorID > 2) && respectFloorCalls && isFloorCalling(2))) {
      return 1;
    }
    
    else if (((currentFloorID > 2) && respectInLiftCalls && floorButtons_2)) {
      return 1;
    }
    
    else if (((currentFloorID > 3) && respectFloorCalls && isFloorCalling(3))) {
      return 1;
    }
    
    else if (((currentFloorID > 3) && respectInLiftCalls && floorButtons_3)) {
      return 1;
    }
    
    else if (((currentFloorID > 4) && respectFloorCalls && isFloorCalling(4))) {
      return 1;
    }
    
    else if (((currentFloorID > 4) && respectInLiftCalls && floorButtons_4)) {
      return 1;
    } 
    else {
      return 0;
    }
  }
}
int stopRequestedInDirection__role__twothirdsfull(int dir , int respectFloorCalls , int respectInLiftCalls )  {
  int overload =  (weight > (maximumWeight * 2 / 3));
  int buttonPressed =  isAnyLiftButtonPressed();
  if ((overload && buttonPressed)) {
    return stopRequestedInDirection__before__twothirdsfull(dir, 0, respectInLiftCalls);
  }  
  else {
    return stopRequestedInDirection__before__twothirdsfull(dir, respectFloorCalls, respectInLiftCalls);
  }
}
int stopRequestedInDirection__before__executivefloor(int dir , int respectFloorCalls , int respectInLiftCalls )  {
  int _292_overload =  (weight > (maximumWeight * 2 / 3));
  int _292_buttonPressed =  isAnyLiftButtonPressed();
  if (id2i_twothirdsfull) {
    id2iperf_time_before("TWOTHIRDSFULL");
    if ((_292_overload && _292_buttonPressed)) {
      (respectFloorCalls = 0);
    }  
    id2iperf_time_after(1);
  }  
  return stopRequestedInDirection__before__twothirdsfull(dir, respectFloorCalls, respectInLiftCalls);
}
int stopRequestedInDirection__role__executivefloor(int dir , int respectFloorCalls , int respectInLiftCalls )  {
  if (isExecutiveFloorCalling()) {
    return ((getCurrentFloorID() < executiveFloor) == (dir == 1));
  }  
  else {
    return stopRequestedInDirection__before__executivefloor(dir, respectFloorCalls, respectInLiftCalls);
  }
}
int stopRequestedInDirection(int dir , int respectFloorCalls , int respectInLiftCalls )  {
  if (id2i_executivefloor) {
    id2iperf_time_before("EXECUTIVEFLOOR");
    if (isExecutiveFloorCalling()) {
      return ((getCurrentFloorID() < executiveFloor) == (dir == 1));
    }  
    id2iperf_time_after(1);
  }  
  return stopRequestedInDirection__before__executivefloor(dir, respectFloorCalls, respectInLiftCalls);
}
int isAnyLiftButtonPressed()  {
  if (floorButtons_0) {
    return 1;
  } 
  else if (floorButtons_1) {
    return 1;
  }
  
  else if (floorButtons_2) {
    return 1;
  }
  
  else if (floorButtons_3) {
    return 1;
  }
  
  else if (floorButtons_4) {
    return 1;
  } 
  else {
    return 0;
  }
}
void continueInDirection(int dir )  {
  (currentHeading = dir);
  if ((currentHeading == 1)) {
    if (isTopFloor(currentFloorID)) {
      (currentHeading = 0);
    }  
  }  
  else {
    if ((currentFloorID == 0)) {
      (currentHeading = 1);
    }  
  }
  if ((currentHeading == 1)) {
    (currentFloorID = (currentFloorID + 1));
  }  
  else {
    (currentFloorID = (currentFloorID - 1));
  }
}
int stopRequestedAtCurrentFloor__before__twothirdsfull()  {
  if (isFloorCalling(currentFloorID)) {
    return 1;
  } 
  else if (buttonForFloorIsPressed(currentFloorID)) {
    return 1;
  } 
  else {
    return 0;
  }
}
int stopRequestedAtCurrentFloor__role__twothirdsfull()  {
  if ((weight > (maximumWeight * 2 / 3))) {
    return (buttonForFloorIsPressed(getCurrentFloorID()) == 1);
  }  
  else {
    return stopRequestedAtCurrentFloor__before__twothirdsfull();
  }
}
int stopRequestedAtCurrentFloor__before__executivefloor()  {
  if (id2i_twothirdsfull) {
    id2iperf_time_before("TWOTHIRDSFULL");
    if ((weight > (maximumWeight * 2 / 3))) {
      return (buttonForFloorIsPressed(getCurrentFloorID()) == 1);
    }  
    id2iperf_time_after(1);
  }  
  return stopRequestedAtCurrentFloor__before__twothirdsfull();
}
int stopRequestedAtCurrentFloor__role__executivefloor()  {
  usleep(10);
  if ((isExecutiveFloorCalling() && (! (executiveFloor == getCurrentFloorID())))) {
    return 0;
  }  
  else {
    return stopRequestedAtCurrentFloor__before__executivefloor();
  }
}
int stopRequestedAtCurrentFloor()  {
  if (id2i_executivefloor) {
    id2iperf_time_before("EXECUTIVEFLOOR");
    usleep(10);
    if ((isExecutiveFloorCalling() && (! (executiveFloor == getCurrentFloorID())))) {
      return 0;
    }  
    id2iperf_time_after(2);
  }  
  return stopRequestedAtCurrentFloor__before__executivefloor();
}
int getReverseHeading(int ofHeading )  {
  if ((ofHeading == 0)) {
    return 1;
  }  
  else {
    return 0;
  }
}
void processWaitingOnFloor(int floorID )  {
  if (isPersonOnFloor(0, floorID)) {
    removePersonFromFloor(0, floorID);
    pressInLiftFloorButton(getDestination(0));
    enterElevator(0);
  }  
  if (isPersonOnFloor(1, floorID)) {
    removePersonFromFloor(1, floorID);
    pressInLiftFloorButton(getDestination(1));
    enterElevator(1);
  }  
  if (isPersonOnFloor(2, floorID)) {
    removePersonFromFloor(2, floorID);
    pressInLiftFloorButton(getDestination(2));
    enterElevator(2);
  }  
  if (isPersonOnFloor(3, floorID)) {
    removePersonFromFloor(3, floorID);
    pressInLiftFloorButton(getDestination(3));
    enterElevator(3);
  }  
  if (isPersonOnFloor(4, floorID)) {
    removePersonFromFloor(4, floorID);
    pressInLiftFloorButton(getDestination(4));
    enterElevator(4);
  }  
  if (isPersonOnFloor(5, floorID)) {
    removePersonFromFloor(5, floorID);
    pressInLiftFloorButton(getDestination(5));
    enterElevator(5);
  }  
  resetCallOnFloor(floorID);
}
void timeShift__before__overloaded()  {
  if (stopRequestedAtCurrentFloor()) {
    (doorState = 1);
    if ((persons_0 && (getDestination(0) == currentFloorID))) {
      leaveElevator(0);
    }  
    if ((persons_1 && (getDestination(1) == currentFloorID))) {
      leaveElevator(1);
    }  
    if ((persons_2 && (getDestination(2) == currentFloorID))) {
      leaveElevator(2);
    }  
    if ((persons_3 && (getDestination(3) == currentFloorID))) {
      leaveElevator(3);
    }  
    if ((persons_4 && (getDestination(4) == currentFloorID))) {
      leaveElevator(4);
    }  
    if ((persons_5 && (getDestination(5) == currentFloorID))) {
      leaveElevator(5);
    }  
    processWaitingOnFloor(currentFloorID);
    resetFloorButton(currentFloorID);
  }  
  else {
    if ((doorState == 1)) {
      (doorState = 0);
    }  
    if (stopRequestedInDirection(currentHeading, 1, 1)) {
      continueInDirection(currentHeading);
    } 
    else if (stopRequestedInDirection(getReverseHeading(currentHeading), 1, 1)) {
      continueInDirection(getReverseHeading(currentHeading));
    } 
    else {
      continueInDirection(currentHeading);
    }
  }
}
void timeShift__role__overloaded()  {
  if ((areDoorsOpen() && (weight > maximumWeight))) {
    (blocked = 1);
  }  
  else {
    (blocked = 0);
    timeShift__before__overloaded();
  }
}
void timeShift()  {
  if (id2i_overloaded) {
    id2iperf_time_before("OVERLOADED");
    if ((areDoorsOpen() && (weight > maximumWeight))) {
      (blocked = 1);
    }  
    else {
      (blocked = 0);
    }
    id2iperf_time_after(1);
  }  
  timeShift__before__overloaded();
}
void printState__before__overloaded()  {
  
}
void printState__role__overloaded()  {
  printState__before__overloaded();
}
void printState()  {
  
}
int existInLiftCallsInDirection(int d )  {
  if ((d == 1)) {
    int i =  0;
    for ((i = (currentFloorID + 1)); (i < 5); i++) {
      if (((i == 0) && floorButtons_0)) {
        return 1;
      } 
      else if (((i == 1) && floorButtons_1)) {
        return 1;
      }
      
      else if (((i == 2) && floorButtons_2)) {
        return 1;
      }
      
      else if (((i == 3) && floorButtons_3)) {
        return 1;
      }
      
      else if (((i == 4) && floorButtons_4)) {
        return 1;
      } 
    }
  } 
  else if ((d == 0)) {
    int i =  0;
    for ((i = (currentFloorID - 1)); (i >= 0); i--) {
      for ((i = (currentFloorID + 1)); (i < 5); i++) {
        if (((i == 0) && floorButtons_0)) {
          return 1;
        } 
        else if (((i == 1) && floorButtons_1)) {
          return 1;
        }
        
        else if (((i == 2) && floorButtons_2)) {
          return 1;
        }
        
        else if (((i == 3) && floorButtons_3)) {
          return 1;
        }
        
        else if (((i == 4) && floorButtons_4)) {
          return 1;
        } 
      }
    }
  } 
  return 0;
}
int executiveFloor =  4;
int isExecutiveFloorCalling()  {
  return isFloorCalling(executiveFloor);
}
int isExecutiveFloor(int floorID )  {
  return (executiveFloor == floorID);
}
int blocked =  0;
int __GUIDSL_ROOT_PRODUCTION;
void select_helpers();
int valid_product();
void select_helpers()  {
  (__GUIDSL_ROOT_PRODUCTION = 1);
}
int valid_product()  {
  if ((id2i_base && (id2i_weight || ((! id2i_twothirdsfull) && ((! id2i_overloaded) || id2i_weight))))) {
    id2iperf_time_before("(BASE && (WEIGHT || (!TWOTHIRDSFULL && (!OVERLOADED || WEIGHT))))");
    id2iperf_return(1, id2iperf_time_after(1));
  }  
  if (((! id2i_base) || ((! id2i_weight) && (id2i_twothirdsfull || (id2i_overloaded && (! id2i_weight)))))) {
    id2iperf_time_before("(!BASE || (!WEIGHT && (TWOTHIRDSFULL || (OVERLOADED && !WEIGHT))))");
    id2iperf_return(0, id2iperf_time_after(1));
  }  
}
int __GUIDSL_ROOT_PRODUCTION;
int select_one();
void select_helpers();
int valid_product();
int isFloorCalling(int floorID );
void resetCallOnFloor(int floorID );
void callOnFloor(int floorID );
int isPersonOnFloor(int person , int floor );
void initPersonOnFloor(int person , int floor );
void removePersonFromFloor(int person , int floor );
int isTopFloor(int floorID );
void processWaitingPersons(int floorID );
void initFloors();
int calls_0;
int calls_1;
int calls_2;
int calls_3;
int calls_4;
int personOnFloor_0_0;
int personOnFloor_0_1;
int personOnFloor_0_2;
int personOnFloor_0_3;
int personOnFloor_0_4;
int personOnFloor_1_0;
int personOnFloor_1_1;
int personOnFloor_1_2;
int personOnFloor_1_3;
int personOnFloor_1_4;
int personOnFloor_2_0;
int personOnFloor_2_1;
int personOnFloor_2_2;
int personOnFloor_2_3;
int personOnFloor_2_4;
int personOnFloor_3_0;
int personOnFloor_3_1;
int personOnFloor_3_2;
int personOnFloor_3_3;
int personOnFloor_3_4;
int personOnFloor_4_0;
int personOnFloor_4_1;
int personOnFloor_4_2;
int personOnFloor_4_3;
int personOnFloor_4_4;
int personOnFloor_5_0;
int personOnFloor_5_1;
int personOnFloor_5_2;
int personOnFloor_5_3;
int personOnFloor_5_4;
int personOnFloor_10_0;
int personOnFloor_10_1;
int personOnFloor_10_2;
int personOnFloor_10_3;
int personOnFloor_10_4;
void initFloors()  {
  (calls_0 = 0);
  (calls_1 = 0);
  (calls_2 = 0);
  (calls_3 = 0);
  (calls_4 = 0);
  (personOnFloor_0_0 = 0);
  (personOnFloor_0_1 = 0);
  (personOnFloor_0_2 = 0);
  (personOnFloor_0_3 = 0);
  (personOnFloor_0_4 = 0);
  (personOnFloor_1_0 = 0);
  (personOnFloor_1_1 = 0);
  (personOnFloor_1_2 = 0);
  (personOnFloor_1_3 = 0);
  (personOnFloor_1_4 = 0);
  (personOnFloor_2_0 = 0);
  (personOnFloor_2_1 = 0);
  (personOnFloor_2_2 = 0);
  (personOnFloor_2_3 = 0);
  (personOnFloor_2_4 = 0);
  (personOnFloor_3_0 = 0);
  (personOnFloor_3_1 = 0);
  (personOnFloor_3_2 = 0);
  (personOnFloor_3_3 = 0);
  (personOnFloor_3_4 = 0);
  (personOnFloor_4_0 = 0);
  (personOnFloor_4_1 = 0);
  (personOnFloor_4_2 = 0);
  (personOnFloor_4_3 = 0);
  (personOnFloor_4_4 = 0);
  (personOnFloor_5_0 = 0);
  (personOnFloor_5_1 = 0);
  (personOnFloor_5_2 = 0);
  (personOnFloor_5_3 = 0);
  (personOnFloor_5_4 = 0);
}
int isFloorCalling(int floorID )  {
  if ((floorID == 0)) {
    return calls_0;
  } 
  else if ((floorID == 1)) {
    return calls_1;
  }
  
  else if ((floorID == 2)) {
    return calls_2;
  }
  
  else if ((floorID == 3)) {
    return calls_3;
  }
  
  else if ((floorID == 4)) {
    return calls_4;
  } 
  return 0;
}
void resetCallOnFloor(int floorID )  {
  if ((floorID == 0)) {
    (calls_0 = 0);
  } 
  else if ((floorID == 1)) {
    (calls_1 = 0);
  }
  
  else if ((floorID == 2)) {
    (calls_2 = 0);
  }
  
  else if ((floorID == 3)) {
    (calls_3 = 0);
  }
  
  else if ((floorID == 4)) {
    (calls_4 = 0);
  } 
}
void callOnFloor(int floorID )  {
  if ((floorID == 0)) {
    (calls_0 = 1);
  } 
  else if ((floorID == 1)) {
    (calls_1 = 1);
  }
  
  else if ((floorID == 2)) {
    (calls_2 = 1);
  }
  
  else if ((floorID == 3)) {
    (calls_3 = 1);
  }
  
  else if ((floorID == 4)) {
    (calls_4 = 1);
  } 
}
int isPersonOnFloor(int person , int floor )  {
  if ((floor == 0)) {
    if ((person == 0)) {
      return personOnFloor_0_0;
    } 
    else if ((person == 1)) {
      return personOnFloor_1_0;
    }
    
    else if ((person == 2)) {
      return personOnFloor_2_0;
    }
    
    else if ((person == 3)) {
      return personOnFloor_3_0;
    }
    
    else if ((person == 4)) {
      return personOnFloor_4_0;
    }
    
    else if ((person == 5)) {
      return personOnFloor_5_0;
    } 
  } 
  else if ((floor == 1)) {
    if ((person == 0)) {
      return personOnFloor_0_1;
    } 
    else if ((person == 1)) {
      return personOnFloor_1_1;
    }
    
    else if ((person == 2)) {
      return personOnFloor_2_1;
    }
    
    else if ((person == 3)) {
      return personOnFloor_3_1;
    }
    
    else if ((person == 4)) {
      return personOnFloor_4_1;
    }
    
    else if ((person == 5)) {
      return personOnFloor_5_1;
    } 
  }
  
  else if ((floor == 2)) {
    if ((person == 0)) {
      return personOnFloor_0_2;
    } 
    else if ((person == 1)) {
      return personOnFloor_1_2;
    }
    
    else if ((person == 2)) {
      return personOnFloor_2_2;
    }
    
    else if ((person == 3)) {
      return personOnFloor_3_2;
    }
    
    else if ((person == 4)) {
      return personOnFloor_4_2;
    }
    
    else if ((person == 5)) {
      return personOnFloor_5_2;
    } 
  }
  
  else if ((floor == 3)) {
    if ((person == 0)) {
      return personOnFloor_0_3;
    } 
    else if ((person == 1)) {
      return personOnFloor_1_3;
    }
    
    else if ((person == 2)) {
      return personOnFloor_2_3;
    }
    
    else if ((person == 3)) {
      return personOnFloor_3_3;
    }
    
    else if ((person == 4)) {
      return personOnFloor_4_3;
    }
    
    else if ((person == 5)) {
      return personOnFloor_5_3;
    } 
  }
  
  else if ((floor == 4)) {
    if ((person == 0)) {
      return personOnFloor_0_4;
    } 
    else if ((person == 1)) {
      return personOnFloor_1_4;
    }
    
    else if ((person == 2)) {
      return personOnFloor_2_4;
    }
    
    else if ((person == 3)) {
      return personOnFloor_3_4;
    }
    
    else if ((person == 4)) {
      return personOnFloor_4_4;
    }
    
    else if ((person == 5)) {
      return personOnFloor_5_4;
    } 
  } 
  return 0;
}
void initPersonOnFloor(int person , int floor )  {
  if ((floor == 0)) {
    if ((person == 0)) {
      (personOnFloor_0_0 = 1);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_0 = 1);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_0 = 1);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_0 = 1);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_0 = 1);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_0 = 1);
    }
    
    else if ((person == 10)) {
      (personOnFloor_10_0 = 1);
    } 
  } 
  else if ((floor == 1)) {
    if ((person == 0)) {
      (personOnFloor_0_1 = 1);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_1 = 1);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_1 = 1);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_1 = 1);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_1 = 1);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_1 = 1);
    }
    
    else if ((person == 10)) {
      (personOnFloor_10_1 = 1);
    } 
  }
  
  else if ((floor == 2)) {
    if ((person == 0)) {
      (personOnFloor_0_2 = 1);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_2 = 1);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_2 = 1);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_2 = 1);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_2 = 1);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_2 = 1);
    }
    
    else if ((person == 10)) {
      (personOnFloor_10_2 = 1);
    } 
  }
  
  else if ((floor == 3)) {
    if ((person == 0)) {
      (personOnFloor_0_3 = 1);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_3 = 1);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_3 = 1);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_3 = 1);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_3 = 1);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_3 = 1);
    }
    
    else if ((person == 10)) {
      (personOnFloor_10_3 = 1);
    } 
  }
  
  else if ((floor == 4)) {
    if ((person == 0)) {
      (personOnFloor_0_4 = 1);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_4 = 1);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_4 = 1);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_4 = 1);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_4 = 1);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_4 = 1);
    }
    
    else if ((person == 10)) {
      (personOnFloor_10_4 = 1);
    } 
  } 
  callOnFloor(floor);
}
void removePersonFromFloor(int person , int floor )  {
  if ((floor == 0)) {
    if ((person == 0)) {
      (personOnFloor_0_0 = 0);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_0 = 0);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_0 = 0);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_0 = 0);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_0 = 0);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_0 = 0);
    } 
  } 
  else if ((floor == 1)) {
    if ((person == 0)) {
      (personOnFloor_0_1 = 0);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_1 = 0);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_1 = 0);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_1 = 0);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_1 = 0);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_1 = 0);
    } 
  }
  
  else if ((floor == 2)) {
    if ((person == 0)) {
      (personOnFloor_0_2 = 0);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_2 = 0);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_2 = 0);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_2 = 0);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_2 = 0);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_2 = 0);
    } 
  }
  
  else if ((floor == 3)) {
    if ((person == 0)) {
      (personOnFloor_0_3 = 0);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_3 = 0);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_3 = 0);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_3 = 0);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_3 = 0);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_3 = 0);
    } 
  }
  
  else if ((floor == 4)) {
    if ((person == 0)) {
      (personOnFloor_0_4 = 0);
    } 
    else if ((person == 1)) {
      (personOnFloor_1_4 = 0);
    }
    
    else if ((person == 2)) {
      (personOnFloor_2_4 = 0);
    }
    
    else if ((person == 3)) {
      (personOnFloor_3_4 = 0);
    }
    
    else if ((person == 4)) {
      (personOnFloor_4_4 = 0);
    }
    
    else if ((person == 5)) {
      (personOnFloor_5_4 = 0);
    } 
  } 
  resetCallOnFloor(floor);
}
int isTopFloor(int floorID )  {
  return (floorID == (5 - 1));
}
int __GUIDSL_ROOT_PRODUCTION;
int select_one();
void select_helpers();
int valid_product();
int getWeight(int person );
int getOrigin(int person );
int getDestination(int person );
void enterElevator(int person );
int getWeight(int person )  {
  if ((person == 0)) {
    return 40;
  } 
  else if ((person == 1)) {
    return 40;
  }
  
  else if ((person == 2)) {
    return 40;
  }
  
  else if ((person == 3)) {
    return 40;
  }
  
  else if ((person == 4)) {
    return 30;
  }
  
  else if ((person == 5)) {
    return 150;
  } 
  else {
    return 0;
  }
}
int getOrigin(int person )  {
  if ((person == 0)) {
    return 4;
  } 
  else if ((person == 1)) {
    return 4;
  }
  
  else if ((person == 2)) {
    return 2;
  }
  
  else if ((person == 3)) {
    return 1;
  }
  
  else if ((person == 4)) {
    return 0;
  }
  
  else if ((person == 5)) {
    return 1;
  }
  
  else if ((person == 10)) {
    return 4;
  } 
  else {
    return 0;
  }
}
int getDestination(int person )  {
  if ((person == 0)) {
    return 0;
  } 
  else if ((person == 1)) {
    return 0;
  }
  
  else if ((person == 2)) {
    return 1;
  }
  
  else if ((person == 3)) {
    return 3;
  }
  
  else if ((person == 4)) {
    return 1;
  }
  
  else if ((person == 5)) {
    return 3;
  } 
  else {
    return 0;
  }
}
void bigMacCall()  {
  initPersonOnFloor(5, getOrigin(5));
}
int __GUIDSL_ROOT_PRODUCTION;
int cleanupTimeShifts =  12;
void cleanup()  {
  timeShift();
  int i;
  for ((i = 0); ((i < (cleanupTimeShifts - 1)) && (isBlocked() != 1)); i++) {
    if (isIdle()) {
      return;
    }  
    else {
      timeShift();
    }
  }
}
void select_helpers();
int valid_product();
void test()  {
  bigMacCall();
  cleanup();
}
int get_nondet()  {
  int nd;
  return nd;
}
int get_nondetMinMax07()  {
  int nd;
  if ((nd == 0)) {
    return 0;
  } 
  else if ((nd == 1)) {
    return 1;
  }
  
  else if ((nd == 2)) {
    return 2;
  }
  
  else if ((nd == 3)) {
    return 3;
  }
  
  else if ((nd == 4)) {
    return 4;
  }
  
  else if ((nd == 5)) {
    return 5;
  }
  
  else if ((nd == 6)) {
    return 6;
  }
  
  else if ((nd == 7)) {
    return 7;
  } 
  else {
    exit(0);
  }
}
void bobCall()  {
  initPersonOnFloor(0, getOrigin(0));
}
void bobCall2()  {
  initPersonOnFloor(10, getOrigin(10));
}
void aliceCall()  {
  initPersonOnFloor(1, getOrigin(1));
}
void angelinaCall()  {
  initPersonOnFloor(2, getOrigin(2));
}
void chuckCall()  {
  initPersonOnFloor(3, getOrigin(3));
}
void monicaCall()  {
  initPersonOnFloor(4, getOrigin(4));
}
void threeTS()  {
  timeShift();
  timeShift();
  timeShift();
}
void randomSequenceOfActions()  {
  int maxLength =  4;
  if (get_nondet()) {
    initTopDown();
  }  
  else {
    initBottomUp();
  }
  int counter =  0;
  while ((counter < maxLength)) {
    counter++;
    int action =  get_nondetMinMax07();
    if ((action < 6)) {
      int origin =  getOrigin(action);
      initPersonOnFloor(action, origin);
    } 
    else if ((action == 6)) {
      timeShift();
    }
    
    else if ((action == 7)) {
      timeShift();
      timeShift();
      timeShift();
    } 
    if (isBlocked()) {
      return;
    }  
  }
  cleanup();
}
void runTest_Simple()  {
  bigMacCall();
  angelinaCall();
  cleanup();
}
void Specification1()  {
  bigMacCall();
  angelinaCall();
  cleanup();
}
void Specification2()  {
  bigMacCall();
  cleanup();
}
void Specification3()  {
  initBottomUp();
  int i;
  for ((i = 0); (i < 100); i++) {
    bobCall();
    while (isFloorCalling(getOrigin(0))) {
      timeShift();
    }
    timeShift();
    aliceCall();
    while ((! isEmpty())) {
      timeShift();
    }
  }
  cleanup();
}
void setup()  {
  
}
void runTest()  {
  Specification3();
}
int main(void )  {
  id2i_init();
  id2iperf_time_start();
  select_helpers();
  if (valid_product()) {
    setup();
    runTest();
  }  
  id2iperf_return(0, id2iperf_time_end());
}