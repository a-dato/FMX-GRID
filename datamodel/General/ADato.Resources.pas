{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Resources;

interface

uses
  {$IFDEF DELPHI}
  System.Globalization.Interfaces,
  {$ELSE}
  System.Globalization,
  {$ENDIF}
  System_,
  System.Resources;

type
  ADatoResources = {$IFDEF DOTNET}public{$ENDIF} class
  private
    class var resourceCulture: CultureInfo;
    class var resourceMan: ResourceManager;
    class var _lock: CObject;

    class function  get_ResourceManager: ResourceManager; static;
    class function  get_Culture: CultureInfo; static;
    class procedure set_Culture(Value: CultureInfo); static;

  public
    class property Culture: CultureInfo read get_Culture write set_Culture;
    class property ResourceManager: ResourceManager read get_ResourceManager;

    // TDataModelViewDataset
    class function DataModelViewDatasetColumnNotFound: CString;
    class function DataModelViewDatasetNoDataModel: CString;

    // TDataModel
    class function KeyFieldNotSet: CString;
    class function LocateFailed: CString;
    class function NoDatasetToConnect: CString;
    class function NoKey: CString;
    class function RecursiveDataset: CString;

    // ProjectDataModel strings
    class function ProjectDataModelMissingColumnMap: CString;
    class function TaskNotFoundInDataModel: CString;
    class function CannotUpdateTaskStatusCode: CString;

    // Calendar resource strings
    class function &And: CString;
    class function BetweenText: CString;
    class function BufferTooBig: CString;
    class function CantAddSelf: CString;
    class function EndAtText: CString;
    class function EveryText: CString;
    class function StartInvalidInPrepareDateRange: CString;
    class function StopInvalidInPrepareDateRange: CString;
    class function DayNumberNames: CString;
    class function DayOfMonthNames: CString;
    class function NoDataToCopy: CString;
    class function NonWorkingText: CString;
    class function OccurrenceText: CString;
    class function PeriodDay: CString;
    class function PeriodDayTime: CString;
    class function PeriodDayDay: CString;
    class function RecurringDayOfWeek: CString;
    class function RecurringDayOfMonth: CString;
    class function RecurringDayPerMonth: CString;
    class function RecurringDayOfYear: CString;
    class function RecurringDayPerYear: CString;
    class function RulesRequired: CString;
    class function StartAtText: CString;
    class function StartMustBeBeforeStop: CString;
    class function The: CString;
    class function WorkingHourText: CString;
    class function WorkingText: CString;

    // Parser resources
    class function BraceError: CString;
    class function CircularDependency: CString;
    class function FunctionError: CString;
    class function InvalidString: CString;
    class function SyntaxError: CString;
    class function WrongParamCount: CString;

    // Scheduler resource strings
    class function CircularDependencies: CString;
    class function CircularDependencyWithProject: CString;
    class function ConstraintAfterProjectEnd: CString;
    class function ConstraintBeforeProjectStart: CString;
    class function ConstraintRequiresADate: CString;
    class function ConstraintViolation: CString;
    class function FeedingBufferInvalid_NoCCTaskFollowing: CString;
    class function FeedingBufferInvalid_CCTasksFeedingIntoBuffer: CString;
    class function FeedingBufferInvalid_NoTasksFeedingIntoBuffer: CString;
    class function TaskDeadlineViolation: CString;
    class function TaskConstraintDateViolation: CString;
    class function TaskScheduledOutsideStageWindow: CString;
    class function InvalidCombinationOfRequirements: CString;
    class function GlobalSkillsCannotBeAssigned: CString;
    class function IllegalDependency: CString;
    class function DurationNotSetIgnoreTask: CString;
    class function LoadingOffsetGreaterThanDuration: CString;
    class function NoResourceAssignedToTask: CString;
    class function NoRequirementDuration: CString;
    class function MultipleConstraintsInSameChain: CString;
    class function PredecessorDuplicate: CString;
    class function PredecessorNotFound: CString;
    class function ProjectEndDateInvalid: CString;
    class function ProjectDatesAreInvalid: CString;
    class function ProjectStartDateInvalid: CString;
    class function PushOutStart: CString;
    class function PushOutEnd: CString;
    class function DependencyViolation: CString;
    class function ResourceRequirementScheduleFailed: CString;
    class function ResourceMustBeSet: CString;
    class function ResourceMustBeSetOnRequirement: CString;
    class function ResourceUsedMoreThanOnce: CString;
    class function ResourceAlsoAssignedToParent: CString;
    class function RequiredUnitsExceedMaxUnits: CString;
    class function RequiredUnitsExceedMaxUnitsForClass: CString;
    class function TaskHasNoEndDate: CString;
    class function TaskScheduleFailed: CString;
    class function TaskScheduledOutsideWindow: CString;
    class function TasksScheduledBeforeProjectStart: CString;
    class function TasksScheduledAfterProjectEnd: CString;
    class function UnitsMustBeSet: CString;
    class function UnitsMayNotExceed_1: CString;
    class function FinishStart: CString;
    class function StartStart: CString;
    class function FinishFinish: CString;
    class function StartFinish: CString;
    class function DoNotLevel: CString;
    class function MustStartOn: CString;
    class function MustFinishOn: CString;
    class function StartNoEarlierThan: CString;
    class function FinishAtOrBefore: CString;
    class function StartNoLaterThan: CString;
    class function FinishNoLaterThan: CString;
    class function AsSoonAsPossible: CString;
    class function AsLateAsPossible: CString;
    class function Ignore: CString;

    // S95 Scheduler
    class function NoCapableResourcesFound: CString;
    class function SkillUsesDefaultProfile: CString;

    // DBR Scheduler resource strings
    class function ResourceResourceClassNotSet: CString;

    // CC Scheduler resource strings
    class function CannotDetermineCC: CString;
    class function CannotDetermineLCC: CString;
    class function FeedingBuffer: CString;
    class function ProjectBuffer: CString;
    class function ProjectsDoNotMatch: CString;
    class function ResourceBuffer: CString;
    class function RelationMustBeFinishStart: CString;
  end;

implementation

{$R *.res}

uses
  {$IFDEF DELPHI}
  System_.Threading
  {$ENDIF};

Resourcestring
  SAnd = 'and';
  SAsLateAsPossible = 'As late as possible';
  SAsSoonAsPossible = 'As soon as possible';
  SBetweenText = ' between {0} and {1}'; // Mind the space before ''between'';
  SBraceError = 'Braces do not match in ''{0}''.';
  SBufferTooBig = 'Buffer size exceeds available time. This will lengthen the critical chain of the project (see critical chain gap). If this is intentional, the buffer should be set to ''Fixed duration''.';
  SCannotDetermineCC = 'Cannot determine the critical chain for project ''{0}'' due to problems in the project plan. Please resolve these problems, then reschedule your project.';
  SCannotDetermineLCC = 'Cannot determine the current longest chain for project ''{0}'' due to problems in the project plan. Please resolve these problems, then reschedule your project.';
  SCannotUpdateTaskStatusCode = 'Cannot update status code to ''{0}''. Status code may only be updated when project is released.';
  SCantAddSelf = 'You cannot add ''self'' as a source to this profile.';
  SCircularDependencies = 'Dependencies set for task ''{0}'' creates a circular relation ({1}).';
  SCircularDependency = 'Circular dependency between formula''s ''{0}''.';
  SCircularDependencyWithProject = 'Dependencies set for task ''{0}'' in project ''{1}'' creates a circular relation ({2}).';
  SConstraintAfterProjectEnd = 'Constraint date ({0}) lies after project end date ({1:f}).';
  SConstraintBeforeProjectStart = 'Constraint date ({0}) lies before project start date ({1:f}).';
  SConstraintRequiresADate = 'The constraint set for this task requires a constraint date.';
  SConstraintViolation = 'Scheduler could not meet with constraint ''{0}'' set on date {1:f}.';
  SCriticalChainHasChanged = 'The critical chain of your project has changed, therefore exisiting buffers may be invalid.';
  SStartInvalidInPrepareDateRange = 'Start date cannot be set to MinValue when calling Prepare date range and rules are set.';
  SStopInvalidInPrepareDateRange = 'Stop date cannot be set to MaxValue when calling Prepare date range and rules are set.';
  // 	Comma separated list with day number names
  SDayNumberNames = '1st,2nd,3rd,4th,5th,6th,7th,8th,9th,10th,11th,12th,13th,14th,15th,16th,17th,18th,19th,20th,21st,22nd,23rd,24th,25th,26th,27th,28th,29th,30th,31th';
	// Comma separated list with day in month names (2nd tuesday in march)
  SDayOfMonthNames = '1st,2nd,3rd,4th,last';
  SDependencyViolation = 'Scheduler cannot meet with ''{0}'' dependency between ''{1}'' and ''{2}''';
  SDoNotLevel = 'Do not level';
  SDurationNotSetIgnoreTask = 'Duration is not set, task will be ignored while scheduling.';
  SEndAtText = 'ending at {0}';
  SEveryText = 'every {0}';
  SFeedingBuffer = 'Feeding buffer';
  SDataModelViewDatasetColumnNotFound = 'Column of datamodel view is not found';
  SDataModelViewDatasetNoDataModel = 'datamodel does not contain datamodel';
  SFeedingBufferInvalid_CCTasksFeedingIntoBuffer = 'This feeding buffer is invalid because it''s following a critical chain task. Please re-evaluate your buffers by executing the ''recalculate buffers'' procedure from the CCPM menu.';
  SFeedingBufferInvalid_NoCCTaskFollowing = 'This feeding buffer is invalid because there is no critical chain task following it. Please re-evaluate your buffers by executing the ''recalculate buffers'' procedure from the CCPM menu.';
  SFeedingBufferInvalid_NoTasksFeedingIntoBuffer = 'This feeding buffer is invalid because there are no tasks feeding into this buffer. Please re-evaluate your buffers by executing the ''recalculate buffers'' procedure from the CCPM menu.';
  SFinishAtOrBefore = 'Finish no earlier than';
  SFinishFinish = 'Finish -> Finish';
  SFinishNoLaterThan = 'Finish no later than';
  SFinishStart = 'Finish -> Start';
  SFunctionError = 'Error in function ''{0}''.';
  SIgnore = 'Ignore task';
  SIllegalDependency = 'Dependency set between task ''{0}'' and ''{1}'' is illegal and therefore ignored by the scheduler.';
  SInvalidCombinationOfRequirements = 'The combination of resource assignments does not return a valid combination of resources.' + #13#10 +
                                      'Resource assignments are ignored, the task has been scheduled using the project calendar instead.';
  SGlobalSkillsCannotBeAssigned = 'Global skill ''{0}'' cannot be assigned to a task, assignment will be ignored.';
  SPlease = 'verify the resource requirements, probably there are not enough resources supporting the same skill.';
  SThis = 'task has been scheduled against the project calendar any resource assignments are ignored."';
  SInvalidString = 'String is invalid.';
  SKeyFieldNotSet = 'Name of key field not set in datalink ''{0}''.';
  SLoadingOffsetGreaterThanDuration = 'Loading offset + duration is greater than longest duration set for this task in resource assignment {0}';
  SLocateFailed = 'Failed to locate record in dataset ''{0}'' using key ''{1}''.';
  SMultipleConstraintsInSameChain = 'A milestone buffer will not be inserted for this task because this task is proteced by another milestone buffer located in the same chain.';
  SMustFinishOn = 'Must finish on';
  SMustStartOn = 'Must start on';
  SNoCapableResourcesFound = 'None of the resources supports resource class ''{0}'' based on the properties given.';
  SNoDatasetToConnect = 'There is no dataset to connect to.';
  SNoDataToCopy = '''Profile.Sources'' is empty, no data can be copied.';
  SNoKey = 'Key may not be null.';
  SNonWorkingText = 'not working';
  SNoRequirementDuration = 'Duration not set for requirement ''{0}'', hence this requirement will be ingored.';
  SNoResourceAssignedToTask = 'No resource requirements set for task ''{0}''.';
  SOccurrenceText = 'for {0} occurences';
  SPeriodDay = 'Resources are {0} on {1}';
  SPeriodDayDay = 'Resources are {0} between {1} and {2}';
  SPeriodDayTime = 'Resources are {0} on {1} between {2} and {3}';
  SPredecessorDuplicate = 'Duplicate dependency between predecessor task ''{0}'' and successor task ''{1}'' (does one of the parents already define this relation?).';
  SPredecessorNotFound = 'Scheduler could not locate predecessor ''{0}'' for task ''{1}''. Therefore this relation is ignored.';
  SProjectBuffer = 'Project buffer';
  SProjectDataModelMissingColumnMap = 'Project datamodel could not map property ''{0}'' to a column in the task datamodel. Please update your mappings for this column.';
  SProjectDatesAreInvalid = 'Project start date must lie before project end date in project {0}';
  SProjectEndDateInvalid = 'Project end date not set in project ''{0}''';
  SProjectsDoNotMatch = 'Task ''{0}'' and task ''{1}'' must be located in the same project.';
  SProjectStartDateInvalid = 'Project start date not set in project ''{0}''';
  SPushOutEnd = 'End date of project has been updated to {0}.';
  SPushOutStart = 'Start date of project has been updated to {0}.';
  SRecurringDayOfMonth = 'Resources are {0} on the {1} day of {2}month{3}{4}';
  SRecurringDayOfWeek = 'Resources are {0} on {1}{2}{3}{4}';
  SRecurringDayOfYear = 'Resources are {0} on every {1} {2}{3}{4}';
  SRecurringDayPerMonth = 'Resources are {0} on every {1} {2} of {3}month{4}{5}';
  SRecurringDayPerYear = 'Resources are {0} on every {1} {2} in {3}{4}{5}';
  SRecursiveDataset = 'Detail dataset may not be the same as this this dataset.';
  SRelationMustBeFinishStart = 'Critical Chain scheduler can only handle finish -> start relations (''{0}'' -> ''{1}'').';
  SRequiredUnitsExceedMaxUnits = 'The total units required for skill ''{0}'' exceeds the maximum availability set for this skill under Project properties | Resource availability. Task will not be scheduled.';
  SRequiredUnitsExceedMaxUnitsForClass = 'The total units required for skill ''{0}'' exceeds the maximum availability set for this skill under ''Skill dialog | # of resources available''. Task will not be scheduled.';
  SResourceAlsoAssignedToParent = 'Requirment ignored, resource ''{0}'' cannot be assigned to the task and it''s parent.';
  SResourceBuffer = 'Resource buffer';
  SResourceMustBeSet = 'Resource was not set in resource requirement on task ''{0}''. Scheduler wil ignore this task.';
  SResourceMustBeSetOnRequirement = 'Resource was not set in resource requirement of type ''R''.';
  SResourceRequirementRequired = 'ISA-95 scheduler requires each task to have at least 1 resource requirement. Task ''{0}'' has none.';
  SResourceRequirementScheduleFailed = 'Scheduler could not allocate enough availability for requirement ''{0}'' over the interval {1} - {2}';
  SResourceResourceClassNotSet = 'Requirement needs either Resource or ResourceClass to be set.';
  SResourceUsedMoreThanOnce = 'Requirment ignored, resource ''{0}'' is assigned more than once to this task.';
  SRulesRequired = 'No rules assigned to this profile.';
  SSkillUsesDefaultProfile = 'Critical chain projects require the skill availability to be set for skill ''{0}''. Please update your skill information using the Skill dialog box.';
  SStartAtText = 'starting at {0}';
  SStartFinish = 'Start -> Finish';
  SStartMustBeBeforeStop = 'Startdate must lie before stopdate.';
  SStartNoEarlierThan = 'Start no earlier than';
  SStartNoLaterThan = 'Start no later than';
  SStartStart = 'Start -> Start';
  SSyntaxError = 'Syntax error.';
  STaskDeadlineViolation = 'Scheduler could not meet with deadline set on date {0:f}.';
  STaskConstraintDateViolation = 'Scheduler could not meet with constraint set on {0:f}.';
  STaskHasNoEndDate = 'Task can''t be scheduled because there is no end date to schedule from. Either set the end date for your project or add a constraint to this task.';
  STaskNotFoundInDataModel = 'Task ''{0}'' could not be located in datamodel.';
  STaskScheduledOutsideStageWindow = 'This task is scheduled outside the stage boundaries ({0} - {1})';
  STaskScheduledOutsideWindow = 'Task has been scheduled outside it''s schedule window ({0} - {1})';
  STaskScheduleFailed = 'Failed to schedule this task.' + #13#10 +
                        'Resource assignments are ignored, the task has been scheduled using the project calendar instead.';
  STasksScheduledAfterProjectEnd = 'Project is scheduled after the project end date: {1:D}';
  STasksScheduledBeforeProjectStart = 'Project is scheduled before the project start date: {1:D}';
  SThe = 'the 	Mind the space after ''the''';
  SUnitsMayNotExceed_1 = 'Value of ''units'' cannot exceed ''1''. Requirement ''{0}'' will be scheduled at 100% resource allocation.';
  SUnitsMustBeSet = 'Value of ''units'' must be greater than 0 for requirement ''{0}'', hence this requirement will be ingored.';
  SWorkingHourText = 'Resources are normally working between {0} and {1}';
  SWorkingText = 'working';
  SWrongParamCount = 'Wrong number of parameters.';

{ ADatoResources }

class function ADatoResources.get_Culture: CultureInfo;
begin
  Result := resourceCulture;
end;

class function ADatoResources.get_ResourceManager: ResourceManager;
begin
  if resourceMan = nil then
  begin
    {$IFDEF DOTNET}
      if System.Object.ReferenceEquals(resourceMan, nil) then begin
          var temp: System.Resources.ResourceManager := new System.Resources.ResourceManager('ADatoScheduler.Properties.ADato.Resources.VS', typeOf(ADatoResources).Assembly);
          resourceMan := temp;
      end;
      exit(resourceMan);
    {$ELSE}
      Lock(resourceMan);
      begin
        resourceMan := System.Resources.ResourceManager.Create('ADato_Resources', nil {Assembly = nil ==> Load resources from running instance });
        _lock := CObject.Create(resourceMan, True);
      end;
    {$ENDIF}
  end;

  Result := resourceMan;
end;

class function ADatoResources.LoadingOffsetGreaterThanDuration: CString;
begin
  Result := SLoadingOffsetGreaterThanDuration;
end;

class function ADatoResources.KeyFieldNotSet: CString;
begin
  Result := SKeyFieldNotSet;
end;

class function ADatoResources.LocateFailed: CString;
begin
  Result := SLocateFailed;
end;

class function ADatoResources.NoDatasetToConnect: CString;
begin
  Result := SNoDatasetToConnect;
end;

class function ADatoResources.NoKey: CString;
begin
  Result := SNoKey;
end;

class function ADatoResources.RecursiveDataset: CString;
begin
  Result := SRecursiveDataset;
end;

class procedure ADatoResources.set_Culture(Value: CultureInfo);
begin
  resourceCulture := Value;
end;

class function ADatoResources.EveryText: CString;
begin
  Result := SEveryText;
end;

class function ADatoResources.CannotDetermineCC: CString;
begin
  Result := SCannotDetermineCC;
end;

class function ADatoResources.CannotDetermineLCC: CString;
begin
  Result := SCannotDetermineLCC;
end;

class function ADatoResources.FeedingBuffer: CString;
begin
  Result := SFeedingBuffer;
end;

class function ADatoResources.DataModelViewDatasetColumnNotFound: CString;
begin
  Result := SDataModelViewDatasetColumnNotFound;
end;

class function ADatoResources.DataModelViewDatasetNoDataModel: CString;
begin
  Result := SDataModelViewDatasetNoDataModel;
end;

class function ADatoResources.StartInvalidInPrepareDateRange: CString;
begin
  Result := SStartInvalidInPrepareDateRange;
end;

class function ADatoResources.StopInvalidInPrepareDateRange: CString;
begin
  Result := SStopInvalidInPrepareDateRange;
end;

class function ADatoResources.DayNumberNames: CString;
begin
  Result := SDayNumberNames;
end;

class function ADatoResources.DayOfMonthNames: CString;
begin
  Result := SDayOfMonthNames;
end;

class function ADatoResources.DurationNotSetIgnoreTask: CString;
begin
  Result := SDurationNotSetIgnoreTask;
end;

class function ADatoResources.IllegalDependency: CString;
begin
  Result := SIllegalDependency;
end;

class function ADatoResources.InvalidCombinationOfRequirements: CString;
begin
  Result := SInvalidCombinationOfRequirements;
end;

class function ADatoResources.GlobalSkillsCannotBeAssigned: CString;
begin
  Result := SGlobalSkillsCannotBeAssigned;
end;

class function ADatoResources.FeedingBufferInvalid_NoCCTaskFollowing: CString;
begin
  Result := SFeedingBufferInvalid_NoCCTaskFollowing;
end;

class function ADatoResources.FeedingBufferInvalid_CCTasksFeedingIntoBuffer: CString;
begin
  Result := SFeedingBufferInvalid_CCTasksFeedingIntoBuffer;
end;

class function ADatoResources.FeedingBufferInvalid_NoTasksFeedingIntoBuffer: CString;
begin
  Result := SFeedingBufferInvalid_NoTasksFeedingIntoBuffer;
end;

class function ADatoResources.&And: CString;
begin
  Result := SAnd;
end;

class function ADatoResources.BetweenText: CString;
begin
  Result := SBetweenText;
end;

class function ADatoResources.BufferTooBig: CString;
begin
  Result := SBufferTooBig;
end;

class function ADatoResources.CantAddSelf: CString;
begin
  Result := SCantAddSelf;
end;

class function ADatoResources.BraceError: CString;
begin
  Result := SBraceError;
end;

class function ADatoResources.CircularDependency: CString;
begin
  Result := SCircularDependency;
end;

class function ADatoResources.FunctionError: CString;
begin
  Result := SFunctionError;
end;

class function ADatoResources.InvalidString: CString;
begin
  Result := SInvalidString;
end;

class function ADatoResources.SyntaxError: CString;
begin
  Result := SSyntaxError;
end;

class function ADatoResources.WrongParamCount: CString;
begin
  Result := SWrongParamCount;
end;

class function ADatoResources.CircularDependencies: CString;
begin
  Result := SCircularDependencies;
end;

class function ADatoResources.CircularDependencyWithProject: CString;
begin
  Result := SCircularDependencyWithProject;
end;

class function ADatoResources.ConstraintAfterProjectEnd: CString;
begin
  Result := SConstraintAfterProjectEnd;
end;

class function ADatoResources.ConstraintBeforeProjectStart: CString;
begin
  Result := SConstraintBeforeProjectStart;
end;

class function ADatoResources.EndAtText: CString;
begin
  Result := SEndAtText;
end;

class function ADatoResources.ConstraintRequiresADate: CString;
begin
  Result := SConstraintRequiresADate;
end;

class function ADatoResources.ConstraintViolation: CString;
begin
  Result := SConstraintViolation;
end;

class function ADatoResources.TaskDeadlineViolation: CString;
begin
  Result := STaskDeadlineViolation;
end;

class function ADatoResources.TaskConstraintDateViolation: CString;
begin
  Result := STaskConstraintDateViolation;
end;

class function ADatoResources.TaskScheduledOutsideStageWindow: CString;
begin
  Result := STaskScheduledOutsideStageWindow;
end;

class function ADatoResources.NoDataToCopy: CString;
begin
  Result := SNoDataToCopy;
end;

class function ADatoResources.NonWorkingText: CString;
begin
  Result := SNonWorkingText;
end;

class function ADatoResources.NoCapableResourcesFound: CString;
begin
  Result := SNoCapableResourcesFound;
end;

class function ADatoResources.SkillUsesDefaultProfile: CString;
begin
  Result := SSkillUsesDefaultProfile;
end;

class function ADatoResources.NoRequirementDuration: CString;
begin
  Result := SNoRequirementDuration;
end;

class function ADatoResources.MultipleConstraintsInSameChain: CString;
begin
  Result := SMultipleConstraintsInSameChain;
end;

class function ADatoResources.NoResourceAssignedToTask: CString;
begin
  Result := SNoResourceAssignedToTask;
end;

class function ADatoResources.OccurrenceText: CString;
begin
  Result := SOccurrenceText;
end;

class function ADatoResources.PeriodDay: CString;
begin
  Result := SPeriodDay;
end;

class function ADatoResources.PeriodDayTime: CString;
begin
  Result := SPeriodDayTime;
end;

class function ADatoResources.PredecessorDuplicate: CString;
begin
  Result := SPredecessorDuplicate;
end;

class function ADatoResources.PredecessorNotFound: CString;
begin
  Result := SPredecessorNotFound;
end;

class function ADatoResources.ProjectDataModelMissingColumnMap: CString;
begin
  Result := SProjectDataModelMissingColumnMap;
end;

class function ADatoResources.ProjectDatesAreInvalid: CString;
begin
  Result := SProjectDatesAreInvalid;
end;

class function ADatoResources.ProjectEndDateInvalid: CString;
begin
  Result := SProjectEndDateInvalid;
end;

class function ADatoResources.ProjectBuffer: CString;
begin
  Result := SProjectBuffer;
end;

class function ADatoResources.ProjectsDoNotMatch: CString;
begin
  Result := SProjectsDoNotMatch;
end;

class function ADatoResources.ProjectStartDateInvalid: CString;
begin
  Result := SProjectStartDateInvalid;
end;

class function ADatoResources.PushOutStart: CString;
begin
  Result := SPushOutStart;
end;

class function ADatoResources.PushOutEnd: CString;
begin
  Result := SPushOutEnd;
end;

class function ADatoResources.PeriodDayDay: CString;
begin
  Result := SPeriodDayDay;
end;

class function ADatoResources.RecurringDayOfWeek: CString;
begin
  Result := SRecurringDayOfWeek;
end;

class function ADatoResources.RecurringDayOfMonth: CString;
begin
  Result := SRecurringDayOfMonth;
end;

class function ADatoResources.RecurringDayPerMonth: CString;
begin
  Result := SRecurringDayPerMonth;
end;

class function ADatoResources.RecurringDayOfYear: CString;
begin
  Result := SRecurringDayOfYear;
end;

class function ADatoResources.RecurringDayPerYear: CString;
begin
  Result := SRecurringDayPerYear;
end;

class function ADatoResources.ResourceBuffer: CString;
begin
  Result := SResourceBuffer;
end;

class function ADatoResources.RelationMustBeFinishStart: CString;
begin
  Result := SRelationMustBeFinishStart;
end;

class function ADatoResources.ResourceRequirementScheduleFailed: CString;
begin
  Result := SResourceRequirementScheduleFailed;
end;

class function ADatoResources.ResourceMustBeSet: CString;
begin
  Result := SResourceMustBeSet;
end;

class function ADatoResources.ResourceUsedMoreThanOnce: CString;
begin
  Result := SResourceUsedMoreThanOnce;
end;

class function ADatoResources.ResourceAlsoAssignedToParent: CString;
begin
  Result := SResourceAlsoAssignedToParent;
end;

class function ADatoResources.RequiredUnitsExceedMaxUnits: CString;
begin
  Result := SRequiredUnitsExceedMaxUnits;
end;

class function ADatoResources.RequiredUnitsExceedMaxUnitsForClass: CString;
begin
  Result := SRequiredUnitsExceedMaxUnitsForClass;
end;

class function ADatoResources.ResourceResourceClassNotSet: CString;
begin
  Result := SResourceResourceClassNotSet;
end;

class function ADatoResources.RulesRequired: CString;
begin
  Result := SRulesRequired;
end;

class function ADatoResources.TaskHasNoEndDate: CString;
begin
  Result := STaskHasNoEndDate;
end;

class function ADatoResources.StartAtText: CString;
begin
  Result := SStartAtText;
end;

class function ADatoResources.DependencyViolation: CString;
begin
  Result := SDependencyViolation;
end;

class function ADatoResources.StartMustBeBeforeStop: CString;
begin
  Result := SStartMustBeBeforeStop;
end;

class function ADatoResources.TaskNotFoundInDataModel: CString;
begin
  Result := STaskNotFoundInDataModel;
end;

class function ADatoResources.CannotUpdateTaskStatusCode: CString;
begin
  Result := SCannotUpdateTaskStatusCode;
end;

class function ADatoResources.TaskScheduleFailed: CString;
begin
  Result := STaskScheduleFailed;
end;

class function ADatoResources.TaskScheduledOutsideWindow: CString;
begin
  Result := STaskScheduledOutsideWindow;
end;

class function ADatoResources.TasksScheduledBeforeProjectStart: CString;
begin
  Result := STasksScheduledBeforeProjectStart;
end;

class function ADatoResources.TasksScheduledAfterProjectEnd: CString;
begin
  Result := STasksScheduledAfterProjectEnd;
end;

class function ADatoResources.The: CString;
begin
  Result := SThe;
end;

class function ADatoResources.UnitsMustBeSet: CString;
begin
  Result := SUnitsMustBeSet;
end;

class function ADatoResources.UnitsMayNotExceed_1: CString;
begin
  Result := SUnitsMayNotExceed_1;
end;

class function ADatoResources.FinishFinish: CString;
begin
  Result := SFinishFinish;
end;

class function ADatoResources.FinishStart: CString;
begin
  Result := SFinishStart;
end;

class function ADatoResources.StartFinish: CString;
begin
  Result := SStartFinish;
end;

class function ADatoResources.StartStart: CString;
begin
  Result := SStartStart;
end;

class function ADatoResources.DoNotLevel: CString;
begin
  Result := SDoNotLevel;
end;

class function ADatoResources.MustStartOn: CString;
begin
  Result := SMustStartOn;
end;

class function ADatoResources.MustFinishOn: CString;
begin
  Result := SMustFinishOn;
end;

class function ADatoResources.StartNoEarlierThan: CString;
begin
  Result := SStartNoEarlierThan;
end;

class function ADatoResources.FinishAtOrBefore: CString;
begin
  Result := SFinishAtOrBefore;
end;

class function ADatoResources.StartNoLaterThan: CString;
begin
  Result := SStartNoLaterThan;
end;

class function ADatoResources.FinishNoLaterThan: CString;
begin
  Result := SFinishNoLaterThan;
end;

class function ADatoResources.AsSoonAsPossible: CString;
begin
  Result := SAsSoonAsPossible;
end;

class function ADatoResources.AsLateAsPossible: CString;
begin
  Result := SAsLateAsPossible;
end;

class function ADatoResources.Ignore: CString;
begin
  Result := SIgnore;
end;

class function ADatoResources.ResourceMustBeSetOnRequirement: CString;
begin
  Result := SResourceMustBeSetOnRequirement;
end;

class function ADatoResources.WorkingHourText: CString;
begin
  Result := SWorkingHourText;
end;

class function ADatoResources.WorkingText: CString;
begin
  Result := SWorkingText;
end;

end.
