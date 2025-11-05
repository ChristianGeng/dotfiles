# TypeScript Next.js Rules

## Project Structure & Organization
```
src/
├── app/                 # Next.js App Router
│   ├── api/            # API routes
│   ├── globals.css    # Global styles
│   ├── layout.tsx     # Root layout
│   └── page.tsx       # Home page
├── components/         # React components
├── services/          # Business logic & external services
├── types/             # TypeScript type definitions
├── utils/             # Utility functions
└── tests/             # Test files
    ├── api/           # API route tests
    ├── integration/   # Integration tests
    └── unit/          # Unit tests
```

## TypeScript Best Practices

### Type Definitions
- Use interfaces for object shapes, types for unions/primitives
- Export types from dedicated `types/` directory
- Use `export type` for type-only exports
- Prefer explicit return types for public functions

### Component Types
```typescript
// Props interface
interface ComponentProps {
  title: string;
  count?: number;
  onSubmit: (data: FormData) => void;
}

// Component with explicit props type
const MyComponent: React.FC<ComponentProps> = ({ title, count = 0, onSubmit }) => {
  return <div>{title}: {count}</div>;
};
```

### API Route Types
```typescript
// Request/Response types
interface DeleteFileRequest {
  key: string;
}

interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  error?: string;
}

// API route handler with proper typing
export async function DELETE(request: NextRequest): Promise<NextResponse<ApiResponse>> {
  // Implementation
}
```

## Next.js Specific Rules

### App Router Conventions
- Use async Server Components by default
- Use `"use client"` directive only when necessary
- Keep API routes in `src/app/api/` directory
- Use proper HTTP methods (GET, POST, PUT, DELETE)

### Route Handlers
```typescript
export async function GET(request: NextRequest): Promise<NextResponse> {
  try {
    const { searchParams } = new URL(request.url);
    const key = searchParams.get('key');
    
    if (\!key) {
      return NextResponse.json(
        { error: 'Key parameter required' },
        { status: 400 }
      );
    }
    
    const result = await someService(key);
    return NextResponse.json(result);
  } catch (error) {
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
```

### Environment Variables
- Use `.env.local` for local development
- Use `.env.test.local` for testing
- Prefix public variables with `NEXT_PUBLIC_`
- Validate required environment variables

## Testing Rules

### Jest Configuration
- Use ts-jest for TypeScript support
- Separate unit and integration tests
- Use proper test environment (jsdom for components, node for API)
- Mock external dependencies (AWS, Next.js)

### Test Structure
```typescript
describe('ServiceName', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should handle success case', async () => {
    // Arrange
    const mockData = { key: 'test' };
    jest.mocked(someService).mockResolvedValue(mockData);
    
    // Act
    const result = await functionUnderTest('test');
    
    // Assert
    expect(result).toEqual(mockData);
    expect(someService).toHaveBeenCalledWith('test');
  });
});
```

### API Testing
```typescript
describe('/api/endpoint', () => {
  it('should return 400 for missing parameters', async () => {
    const request = new Request('http://localhost:3000/api/endpoint');
    const response = await DELETE(request);
    
    expect(response.status).toBe(400);
    const data = await response.json();
    expect(data.error).toBe('Parameter required');
  });
});
```

## ESLint Configuration
```json
{
  "extends": ["next/core-web-vitals", "next/typescript"],
  "rules": {
    "@typescript-eslint/no-unused-vars": "error",
    "@typescript-eslint/explicit-function-return-type": "warn",
    "prefer-const": "error",
    "no-var": "error",
    "react-hooks/rules-of-hooks": "error",
    "react-hooks/exhaustive-deps": "warn"
  }
}
```

## Development Workflow

### Package Scripts
```json
{
  "scripts": {
    "dev": "next dev --turbopack",
    "build": "next build --turbopack",
    "test": "jest",
    "test:unit": "jest --testPathIgnorePatterns=tests/integration",
    "test:integration": "jest --testPathPattern=tests/integration --runInBand",
    "verify": "rm -rf .next && tsc --noEmit && eslint . && next build",
    "format": "eslint --fix . --ext .ts,.tsx,.js,.jsx"
  }
}
```

### Git Hooks (Recommended)
```json
{
  "husky": {
    "hooks": {
      "pre-commit": "npm run verify",
      "pre-push": "npm run test"
    }
  }
}
```

## Best Practices

### Performance
- Use dynamic imports for large components
- Implement proper caching strategies
- Optimize images with Next.js Image component
- Use React.memo for expensive components

### Security
- Validate all user inputs
- Use proper CORS configuration
- Sanitize environment variables
- Implement rate limiting for API routes

### Error Handling
```typescript
// Proper error boundary
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(error) {
    return { hasError: true };
  }

  componentDidCatch(error, errorInfo) {
    console.error('Error caught by boundary:', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return <h1>Something went wrong.</h1>;
    }
    return this.props.children;
  }
}
```

## Code Style

### Naming Conventions
- Components: PascalCase (MyComponent)
- Functions: camelCase (getUserData)
- Constants: UPPER_SNAKE_CASE (API_BASE_URL)
- Files: kebab-case for utilities, PascalCase for components

### Import Organization
```typescript
// 1. External libraries
import React from 'react';
import { S3Client } from '@aws-sdk/client-s3';

// 2. Internal modules (alias with @)
import { ApiResponse } from '@/types';
import { deleteUser } from '@/services/userService';

// 3. Relative imports
import './Component.css';
```

### Component Structure
```typescript
// 1. Types/interfaces
interface Props {
  // prop definitions
}

// 2. Component implementation
const Component: React.FC<Props> = ({ prop1, prop2 }) => {
  // 3. Hooks (useState, useEffect, etc.)
  const [state, setState] = useState<string>('');
  
  // 4. Event handlers
  const handleClick = useCallback(() => {
    // handler logic
  }, [dependencies]);
  
  // 5. Effects
  useEffect(() => {
    // effect logic
  }, [dependencies]);
  
  // 6. Derived values
  const computedValue = useMemo(() => {
    return state.toUpperCase();
  }, [state]);
  
  // 7. Render
  return (
    <div>
      {/* JSX */}
    </div>
  );
};

export default Component;
```




